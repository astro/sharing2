module Application where

import Yesod hiding (fileName)
import Text.Hamlet
import qualified Network.Wai as Wai
import System.IO
import Control.Applicative
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad
import Yesod.Static
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import Control.Monad.Trans.Resource (register)
import Data.IORef
import Data.Time.Clock.POSIX
import Control.Concurrent

import Storage
import Upload
import Tokens


data Settings = Settings
    { settingsPort :: Int
    }
    
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
    { sharingSettings :: Settings
    , sharingStorage :: Storage
    , sharingTokens :: TokenPool TokenState
    , getStatic :: Static
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
    maximumContentLength _ _ = 1024 ^ 3
    
getHomeR :: GHandler sub Sharing RepHtml
getHomeR =
  do files <- sharingStorage <$> getYesod >>= liftIO . allFiles
     showAge <- liftIO getPOSIXTime >>= \now ->
                return $ formatAge . (now -)
     defaultLayout $ do
       setTitle "Sharing is Caring"
       toWidget [hamlet|
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
                            ns | n' < 10 = show $
                                           fromIntegral (truncate $ n' * 10) / 10
                               | otherwise = show $ truncate n'
                        in ns ++ " " ++ unit ++ "B"

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

postUploadR :: Token -> GHandler sub Sharing RepHtml
postUploadR token =
    do waiReq <- reqWaiRequest <$> getRequest
       
       storage <- sharingStorage <$> getYesod
       file <- liftIO $ newFile storage
       committed <- liftIO $ newIORef False
       lift $ register $
            do committed' <- readIORef committed
               when (not committed') $
                    uFileDiscard file
       
       progress <- liftIO $ newUploadProgress
       tokenUploadedHandler <- createTokenHandlers progress

       mFileInfo <-
         lift $ 
         acceptUpload waiReq progress $ uFilePath file
       
       case mFileInfo of
         Just (name, type_, size) -> 
             do mDescription <- tokenUploadedHandler (uFileId file) (bToT name)
                liftIO $ do
                        uFileCommit file (bToT name) (bToT type_) size mDescription
                        writeIORef committed True
         _ ->
             -- Token will be deleted by register'ed tokenCleaner
             return ()
             
       defaultLayout $ do
         setTitle "Uploaded!"
         toWidget [hamlet|
                   <p>
                     Thanks. #
                     <a href=@{HomeR}>Return!
                   |]
           
    where bToT = T.pack . BC.unpack
          
          useToken = not $ T.null $ unToken token
          
          createTokenHandlers progress
              | not useToken =
                  return $ const $ const $ return Nothing
              | otherwise = 
                  do tokenPool <- sharingTokens <$> getYesod
                     liftIO $ newToken token (Uploading progress Nothing) tokenPool
                     let tokenCleanInterval = 30
                         tokenCleaner = do
                           threadDelay tokenCleanInterval
                           now <- getPOSIXTime
                           let canDelete (Uploaded _ _ lastActivity) =
                                   lastActivity + tokenCleanInterval < now
                               canDelete _ =
                                   True
                           deleted <-
                               mayDeleteToken token canDelete tokenPool
                           hPutStrLn stderr $ "Delete token " ++ show token ++ ": " ++ show deleted
                           when (not deleted)
                                tokenCleaner
                     _ <- register $ do
                            _ <- forkIO $ tokenCleaner
                            return ()

                     let tokenUploadedHandler fId name =
                             do link <- ($ FileR fId name) <$>
                                        getUrlRender
                                mToken <- liftIO $
                                          readToken token tokenPool
                                let mDescription = 
                                        case mToken of
                                          Just (Uploading _ mDescription) ->
                                              mDescription
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
                     return tokenUploadedHandler

postDescribeR :: Token -> GHandler sub Sharing RepJson
postDescribeR token =
    do description <- lookupPostParam "description"
       tokenPool <- sharingTokens <$> getYesod
       mState <-
           liftIO $
           readToken token tokenPool
       storage <- sharingStorage <$> getYesod
       let response = RepJson $ toContent $
                      object ([] :: [(T.Text, String)])
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

  
getProgressR :: Token -> GHandler sub Sharing RepJson
getProgressR token =
    do tokenPool <- sharingTokens <$> getYesod
       mState <-
           liftIO $
           readToken token tokenPool
       case mState of
         Just (Uploading progress _) ->
             do (bytes, rate) <- liftIO $ readUploadProgress progress
                liftIO $ hPrint stderr ("progress", bytes, rate)
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

data RepFile = RepFile T.Text FilePath

instance HasReps RepFile where
    chooseRep (RepFile ct path) _ =
        return ( BC.pack $ T.unpack ct
               , ContentFile path Nothing
               )

-- TODO: evaluate `Range' header for Wai.FilePart
getFileR :: Integer -> T.Text -> GHandler sub Sharing RepFile
getFileR fId fName =
    sharingStorage <$> getYesod >>=
    liftIO . getFile fId fName >>=
    maybe notFound (return . uncurry RepFile)


-- | Main, starts web server
run :: Settings -> IO ()
run settings =
    do storage <- createStorage "files.json" "files"
       tokenPool <- createTokenPool
       s <- static "static"
       warp (settingsPort settings) $
            Sharing settings storage tokenPool s

  
