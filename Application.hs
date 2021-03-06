module Application where

import Yesod hiding (fileName)
import Text.Hamlet
import System.IO
import Control.Applicative
import Control.Monad
import Yesod.Static
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import Control.Monad.Trans.Resource (register)
import Data.IORef
import Data.Time.Clock.POSIX
import Control.Concurrent
import qualified Network.Wai as Wai
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.RequestLogger
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified System.Remote.Monitoring as EKG
import qualified System.Remote.Counter as EKGCounter
import qualified Data.Attoparsec.ByteString.Char8 as PC
import Network.HTTP.Types.Status (ok200, partialContent206)
import Network.Wai.Middleware.HttpAuth

import Storage
import Upload
import Tokens


-- | An uploader with Token may query the state and set the
-- description afterwards
data TokenState = Uploading 
                  { uploadProgress :: UploadProgress
                  , uploadDescription :: Maybe T.Text
                  }
                | Uploaded
                  { uploadFileId :: Integer
                  , uploadLink :: T.Text
                  , uploadLastActivity :: POSIXTime
                  }
                deriving (Show)
    
data Sharing = Sharing
    { sharingStorage :: Storage
    , sharingTokens :: TokenPool TokenState
    , getStatic :: Static
    , countIndex :: IO ()
    , countUp :: IO ()
    , countDown :: IO ()
    }
    
mkYesod "Sharing" [parseRoutes|
                   / HomeR GET
                   /upload/#Token UploadR POST
                   /progress/#Token ProgressR GET
                   /describe/#Token DescribeR POST
                   /file/#Integer/#T.Text FileR GET
                   /static StaticR Static getStatic
                   |]
  
instance Yesod Sharing where
    defaultLayout widget =
        do pc <- widgetToPageContent widget
           hamletToRepHtml $(hamletFile "default-layout.hamlet")

    makeSessionBackend = const $ return Nothing

    -- | TODO
    maximumContentLength _ _ = Just $ 1024 ^ 5
    
getHomeR :: Handler RepHtml
getHomeR =
  do getYesod >>= liftIO . countIndex
     files <- sharingStorage <$> getYesod >>= liftIO . allFiles
     showAge <- liftIO getPOSIXTime >>= \now ->
                return $ formatAge . (now -)
     defaultLayout $ do
       setTitle "Sharing is Caring"
       toWidget [hamlet|
                 $newline always
                 <h1>
                   <a href=@{HomeR}>
                     Sharing is Caring
                 <div #up>
                   <form method="post" 
                         enctype="multipart/form-data"
                         action=@{UploadR $ Token ""}>
                     <input id="file" name="file" type="file" size="8">
                     <input type="submit" value="Upload">
                 <ul #down>
                   $forall file <- files
                     <li>
                       <a href=@{FileR (fileId file) (fileName file)}
                          type=#{fileType file}>#{fileName file}
                       <span .size>#{humanSize $ fileSize file}
                       $maybe description <- fileDescription file
                         <span .desc>#{description}
                       <span .desc>
                       <span .stats>
                         <b>#{fileDownloads file}×</b>
                         \ in #{showAge $ fileDate file}
                 |]
       addScriptRemote "/static/jquery-1.7.2.min.js"
       addScriptRemote "/static/sharing.js"

    where humanSize :: (Integral a, Show a) => a -> String
          humanSize n = let (n', unit) = humanSize' $ fromIntegral n
                            ns | n' < 10 = show $ round10 n'
                               | otherwise = show (truncate n' :: Integer)
                        in ns ++ " " ++ unit ++ "B"
          round10 :: Double -> Double
          round10 n = let n' :: Integer
                          n' = truncate $ n * 10
                      in fromIntegral n' / 10

          humanSize' :: Double -> (Double, String)
          humanSize' n = foldl (\(n', unit) unit' ->
                                    if n' < 1024
                                    then (n', unit)
                                    else (n' / 1024, [unit'])
                               ) (n, "") "KMGT"

          pluralize :: Integer -> String -> String
          pluralize 1 w = "1 " ++ w
          pluralize n w = show n ++ " " ++ w ++ "s"
          formatAge :: POSIXTime -> String
          formatAge n = foldl (\r (unit, unitName) ->
                                   if n' < unit
                                   then r
                                   else pluralize (n' `div` unit) unitName
                              ) (pluralize n' "second")
                        [ (60, "minute")
                        , (60 * 60, "hour")
                        , (24 * 60 * 60, "day")
                        , (7 * 24 * 60 * 60, "week")
                        , (30 * 24 * 60 * 60, "month")
                        , (365 * 24 * 60 * 60, "year")
                        , (3650 * 24 * 60 * 60, "decade")
                        ]
              where n' :: Integer
                    n' = truncate n

postUploadR :: Token -> Handler RepHtml
postUploadR token =
    do getYesod >>= liftIO . countUp
       waiReq <- reqWaiRequest <$> getRequest
       
       storage <- sharingStorage <$> getYesod
       file <- liftIO $ newFile storage
       committed <- liftIO $ newIORef False
       _ <- register $
            do committed' <- readIORef committed
               when (not committed') $
                    uFileDiscard file
       
       progress <- liftIO $ newUploadProgress
       tokenUploadedHandler <- createTokenHandlers progress

       -- Consume the upload:
       mFileInfo <-
         lift $ 
         acceptUpload waiReq progress $ uFilePath file
       
       case mFileInfo of
         Just (name, type_, size) -> 
             do let name' = decodeUtf8 name
                mDescription <- tokenUploadedHandler (uFileId file) name'
                liftIO $ do
                        uFileCommit file name' (decodeUtf8 type_) size mDescription
                        writeIORef committed True
         _ ->
             -- Token will be deleted by register'ed tokenCleaner
             invalidArgs ["Missing file"]
             
       defaultLayout $ do
         setTitle "Uploaded!"
         toWidget [hamlet|
                   $newline always
                   <p>
                     Thanks. #
                     <a href=@{HomeR}>Return!
                   |]
           
    where useToken = not $ T.null $ unToken token
          
          tokenLifetime = 30
          tokenCleanInterval = tokenLifetime * 1000 * 1000
          
          createTokenHandlers progress
              | not useToken =
                  return $ const $ const $ return Nothing
              | otherwise = 
                  do 
                    -- 1: Setup
                    tokenPool <- sharingTokens <$> getYesod
                    liftIO $ putStrLn $ "New token: " ++ show token
                    tokenInserted <- liftIO $ newToken token (Uploading progress Nothing) tokenPool
                    when (not tokenInserted) $
                         invalidArgs ["Token collission"]
                     
                    -- 2: Get called when upload data (filename) is
                    -- known, but keep token around for setting description
                    let tokenUploadedHandler fId name =
                            do link <- ($ FileR fId name) <$>
                                       getUrlRender
                               mToken <- liftIO $
                                         readToken token tokenPool
                               let mDescription = 
                                       case mToken of
                                         Just (Uploading _ mDescription') ->
                                             mDescription'
                                         _ ->
                                             Nothing
                               now <- liftIO getPOSIXTime
                               liftIO $
                                      writeToken token 
                                      Uploaded 
                                      { uploadFileId = fId
                                      , uploadLink = link
                                      , uploadLastActivity = now
                                      } tokenPool
                               return mDescription

                    -- 3: Register periodical token cleaner
                    let tokenCleaner = do
                          threadDelay tokenCleanInterval
                          now <- getPOSIXTime
                          let canDelete (Uploaded _ _ lastActivity) =
                                  -- Let token timeout (Client JS
                                  -- periodically refreshes
                                  -- lastActivity)
                                  lastActivity + tokenLifetime < now
                              canDelete _ =
                                  -- Still uploading? register'ed is
                                  -- called after all
                                  -- processing. Delete:
                                  True
                          deleted <-
                              mayDeleteToken token canDelete tokenPool
                          hPutStrLn stderr $ "Delete token " ++ show token ++ ": " ++ show deleted
                          when (not deleted)
                               tokenCleaner
                    -- When the POST request is done, start to
                    -- periodically check if we can drop the token
                    _ <- register $ do
                           _ <- forkIO $ tokenCleaner
                           return ()

                    return tokenUploadedHandler

postDescribeR :: Token -> Handler RepJson
postDescribeR token =
    do description <- lookupPostParam "description"
       tokenPool <- sharingTokens <$> getYesod
       mState <-
           liftIO $
           readToken token tokenPool
       storage <- sharingStorage <$> getYesod
       let response = RepJson $ toContent $
                      object []
       liftIO $ hPutStrLn stderr $ "postDescribeR, tokenState=" ++ show mState
       case mState of
         Just upload@(Uploading _ Nothing) -> 
             do liftIO $
                       writeToken token upload 
                                      { uploadDescription = description 
                                      } tokenPool
                return response
         Just upload@(Uploaded _ _ _) ->
             do liftIO $
                       setDescription (uploadFileId upload) description storage
                return response
         _ ->
             notFound

  
getProgressR :: Token -> Handler RepJson
getProgressR token =
    do tokenPool <- sharingTokens <$> getYesod
       mState <-
           liftIO $
           readToken token tokenPool
       liftIO $ putStrLn $ "token state: " ++ show mState
       case mState of
         Just (Uploading progress _) ->
             do (bytes, rate) <- liftIO $ readUploadProgress progress
                return $ RepJson $ toContent $
                       object [ "bytes" .= bytes
                              , "rate" .= rate
                              ]
         Just upload@(Uploaded _ _ _) -> 
             liftIO $
             do now <- getPOSIXTime
                writeToken token upload { uploadLastActivity = now } tokenPool
                liftIO $ hPutStrLn stderr $ "Token " ++ show token ++ " refreshed to " ++ show now
                return $ RepJson $ toContent $
                       object [ "link" .= uploadLink upload ]
         _ ->
             notFound

getFileR :: Integer -> T.Text -> Handler RepPlain
getFileR fId fName =
    getYesod >>= liftIO . countDown >>
    sharingStorage <$> getYesod >>=
    liftIO . getFile fId fName >>=
    maybe notFound (\(ct, path, size) -> 
                        lookup "Range" <$> 
                        Wai.requestHeaders <$> 
                        reqWaiRequest <$> 
                        getRequest >>= \mRange ->
                        case (mRange, mRange >>= parseRange size) of
                          (Just range, mPart@(Just (Wai.FilePart start size))) ->
                              sendWaiResponse $ Wai.ResponseFile partialContent206 
                                     [("Content-Type", encodeUtf8 $ ct),
                                      ("Content-Range", BC.pack $ "bytes " ++ show start ++ "-" ++ show (start + size - 1) ++ "/" ++ show size)]
                                     path mPart
                          _ ->
                              sendWaiResponse $ Wai.ResponseFile ok200
                                    [("Content-Type", encodeUtf8 $ ct),
                                     ("Content-Length", BC.pack $ show size)]
                                    path Nothing
                   )
    where parseRange size = either (const Nothing) Just . PC.parseOnly (rangeHeader size)
          rangeHeader size =
              do PC.string "bytes="
                 PC.I start <- PC.number
                 PC.char '-'
                 end <- (PC.number >>= \(PC.I end) ->
                         return end) <|> 
                        return (size - 1)
                 return $ Wai.FilePart start $ end - start + 1

-- | Constructs application
app :: IO Application
app =
    EKG.forkServer "localhost" 8081 >>=
    yApp >>=
    toWaiAppPlain >>=
    return . logStdout . autohead . auth
    where yApp ekg =
              do storage <- createStorage "files.json" "files"
                 tokenPool <- createTokenPool
                 s <- static "static"
                 Sharing storage tokenPool s <$>
                         (EKGCounter.inc <$> EKG.getCounter "index" ekg) <*>
                         (EKGCounter.inc <$> EKG.getCounter "up" ekg) <*>
                         (EKGCounter.inc <$> EKG.getCounter "down" ekg)
          auth = basicAuth
                 (\user password ->
                      return $
                      user == "k-ot" && password == "k-ot"
                 )
                 "Sharing is Caring"
