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

import Storage
import Upload
import Tokens


data Settings = Settings
    { settingsPort :: Int
    }
    
data Sharing = Sharing
    { sharingSettings :: Settings
    , sharingStorage :: Storage
    , sharingTokens :: TokenPool UploadProgress
    , getStatic :: Static
    }
    
mkYesod "Sharing" [parseRoutes|
                   / HomeR GET
                   /upload/#Token UploadR POST
                   /progress/#Token ProgressR GET
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
                     <input #submit
                           type="submit" value="Upload">
                 <ul #down>
                   $forall file <- files
                     <li>
                       <a href=@{FileR (fileId file) (fileName file)}
                          type=#{fileType file}>#{fileName file}
                       <span .size>#{humanSize $ fileSize file}
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
          pluralize 1 w = "1 " ++ w
          pluralize n w = show n ++ " " ++ w ++ "s"
          formatAge :: POSIXTime -> String
          formatAge n = foldl (\r (unit, unitName) ->
                                   if n' < unit
                                   then r
                                   else pluralize (n' `div` unit) unitName
                              ) (pluralize n "second")
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
       liftIO $ hPutStrLn stderr $ "headers: " ++ show (Wai.requestHeaders waiReq)
       
       storage <- sharingStorage <$> getYesod
       file <- liftIO $ newFile storage
       committed <- liftIO $ newIORef False
       lift $ register $
            do committed' <- readIORef committed
               when (not committed') $
                    hPutStrLn stderr "discard" >>
                    fileDiscard file
       
       progress <- liftIO $ newUploadProgress
       when (not $ T.null $ unToken token) $ do
         -- TODO: mv progress here
         tokenPool <- sharingTokens <$> getYesod
         liftIO $ newToken tokenPool token progress
         liftIO $ hPutStrLn stderr $ "New token " ++ show token
         --register $ forkIO $ deleteToken tokenPool token
       

       mFileInfo <-
         lift $ 
         acceptUpload waiReq progress $ filePath file
       liftIO $ hPutStrLn stderr $ "acceptUpload -> " ++ show mFileInfo
       
       case mFileInfo of
         Just (name, type_, size) ->
             liftIO $
             do fileCommit file (bToT name) (bToT type_) size
                writeIORef committed True
         _ ->
             -- discarded by action register'ed above
             return ()

       defaultLayout $ do
         setTitle "Uploaded!"
         toWidget [hamlet|
                   <p>
                     Thanks. #
                     <a href=@{HomeR}>Return!
                   |]
           where bToT = T.pack . BC.unpack
    
getProgressR :: Token -> GHandler sub Sharing RepJson
getProgressR token =
    do tokenPool <- sharingTokens <$> getYesod
       mProgress <-
           liftIO $
           readToken tokenPool token
       case mProgress of
         Just progress ->
             do (bytes, rate) <- liftIO $ readUploadProgress progress
                liftIO $ hPrint stderr ("progress", bytes, rate)
                return $ RepJson $ toContent $
                       object [ "bytes" .= bytes
                              , "rate" .= rate
                              ]
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

  
