module Upload where

import Control.Applicative
import Network.Wai
import Network.Wai.Parse
import Data.Conduit
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import Control.Concurrent.STM
import Data.Time.Clock
import Data.Conduit.Binary
import System.IO


data UploadProgress = UploadProgress
                      { uploadStart :: UTCTime
                      , uploadBytes :: TVar Integer
                      }

instance Show UploadProgress where
    show _ = "<<UploadProgress>>"

newUploadProgress :: IO UploadProgress
newUploadProgress = UploadProgress <$>
                    getCurrentTime <*>
                    newTVarIO 0

readUploadProgress :: UploadProgress -> IO (Integer, Double)
readUploadProgress (UploadProgress start tBytes) = 
    do bytes <- readTVarIO tBytes
       now <- getCurrentTime
       return (bytes, 
               unDiffTime $
               (fromIntegral bytes) / (now `diffUTCTime` start))
    where unDiffTime = fromRational . toRational

increaseUploadProgress :: UploadProgress -> Integer -> IO ()
increaseUploadProgress progress delta =
    atomically $
    modifyTVar' (uploadBytes progress) 
                    (+ delta)

-- | sinkRequestBody with our own backend to:
-- 
-- * Write file data directly to the destined location
-- * Update progress every chunk
acceptUpload :: Request -> UploadProgress -> FilePath 
             -> ResourceT IO (Maybe (B.ByteString, B.ByteString, Integer))
acceptUpload waiReq progress newFile =
    do let countBytes =
               await >>=
               maybe (return ())
               (\s ->
                    do liftIO $ 
                              increaseUploadProgress progress $ fromIntegral $ B.length s
                       yield s
                       countBytes
               )
           backend "file" _info =
               do countBytes =$ sinkFile newFile
                  return newFile
           backend _name _info =
               let discard = await >>=
                             maybe (return "")
                             (const discard)
               in discard
       
       case getRequestBodyType waiReq of
         Just rbt ->
             do (_, files) <- requestBody waiReq $$ sinkRequestBody backend rbt
                liftIO $ hPutStrLn stderr $ "sinkRequestBody " ++ show newFile ++ ": " ++ show files
                case "file" `lookup` files of
                  Just fileInfo ->
                      do fileSize <- liftIO $ 
                                     fst <$>
                                     readUploadProgress progress
                         return $ Just ( fileName fileInfo
                                       , fileContentType fileInfo
                                       , fileSize
                                       )
                  Nothing ->
                      return Nothing
         Nothing ->
             return Nothing
             
       