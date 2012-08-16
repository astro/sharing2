module Storage where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Control.Applicative
import Data.Aeson
import qualified Data.Map as Map
import Data.Maybe
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import System.Directory
import System.IO  -- tmp
import Data.Time.Clock.POSIX


-- | Model for stored files
data FileInfo = FileInfo 
    { fileId :: Integer
    , fileName :: T.Text
    , fileType :: T.Text
    , fileDownloads :: Integer
    , fileDate :: POSIXTime
    , fileSize :: Integer
    }
    deriving (Show)

instance FromJSON FileInfo where
  parseJSON (Object v) = FileInfo <$>
                         v .: "id" <*>
                         v .: "name" <*>
                         v .: "type" <*>
                         v .: "downloads" <*>
                         ((/ 1000) . (fromIntegral :: (Integer -> POSIXTime)) <$> 
                          v .: "date") <*>
                         v .: "size"
  parseJSON _          = empty
  
instance ToJSON FileInfo where
  toJSON (FileInfo id name type_ downloads date size) =
         object [ "id" .= id
                , "name" .= name
                , "type" .= type_
                , "downloads" .= downloads
                , "date" .= (truncate (date * 1000) :: Integer)
                , "size" .= size
                ]

newtype Files = Files { unFiles :: [FileInfo] }

instance FromJSON Files where
  parseJSON v@(Array _) = 
      Files <$> 
      parseJSON v
  parseJSON _ = empty
  
instance ToJSON Files where
  toJSON = toJSON . unFiles
  
data Storage = Storage
    { storageNextId :: TVar Integer
    , storageStatePath :: FilePath
    , storageFilesPath :: FilePath
    , storageFiles :: TVar (Map.Map Integer (TVar FileInfo))
    , storageShouldSync :: TVar Bool
    , storageSyncing :: TVar Bool
    }

-- TODO: check files existence
createStorage :: FilePath -> FilePath -> IO Storage
createStorage statePath filesPath =
  do fileInfos <-
         unFiles <$>
         fromMaybe (Files []) <$>
         decode <$> 
         LBC.readFile statePath
     files <-
         Map.fromList <$>
         forM fileInfos 
         (\fi ->
              (fileId fi, ) <$> newTVarIO fi
         )
     let nextId = 1 + maximum (map fileId fileInfos)
         
     seq nextId $
            Storage <$>
            newTVarIO nextId <*> 
            (pure statePath) <*> 
            (pure filesPath) <*> 
            newTVarIO files <*>
            newTVarIO False <*>
            newTVarIO False


shouldSyncStorage :: Storage -> IO ()
shouldSyncStorage = flip maySyncStorage True

maySyncStorage :: Storage -> Bool -> IO ()
maySyncStorage storage should =
    do --hPutStrLn stderr $ "maySyncStorage " ++ show should
       doSync <-
         atomically $ do 
           should' <- (|| should) <$> 
                      readTVar (storageShouldSync storage)
           can <- not <$>
                  readTVar (storageSyncing storage)
           case should' && can of
             True ->
                 do writeTVar (storageShouldSync storage) False
                    writeTVar (storageSyncing storage) True
                    return True
             False ->
                 do writeTVar (storageShouldSync storage) should'
                    return False

       when doSync $ do
         forkIO $ do
           doSyncStorage storage
           -- Release the lock
           atomically $
                  writeTVar (storageSyncing storage) False
           -- Has anything happened in between?
           maySyncStorage storage False
         return ()
       -- Otherwise there is nothing to do, or a sync was already in
       -- progress, picking up the newly set storageShouldSync
       -- afterwards

doSyncStorage :: Storage -> IO ()
doSyncStorage storage = 
    do hPutStrLn stderr "doSyncStorage"
       files <- atomically $
                readTVar (storageFiles storage) >>=
                mapM (readTVar . snd) . Map.toList
       -- Write to .tmp file because anything could crash in between
       let statePath = storageStatePath storage
           statePathTmp = statePath ++ ".tmp"
       LBC.writeFile statePathTmp $ encode files
       -- Success, overwrite with .tmp file
       renameFile statePathTmp statePath

data UploadingFile = UploadingFile
    { filePath :: FilePath
    , fileCommit :: T.Text -> T.Text -> Integer -> IO ()
    , fileDiscard :: IO ()
    }

-- TODO: discard old files
newFile :: Storage -> IO UploadingFile
newFile storage =
    do nextId <-
           atomically $ do
             nextId <- readTVar $ storageNextId storage
             let nextId' = nextId + 1
             nextId' `seq`
                     writeTVar (storageNextId storage) nextId'
             return nextId
       hPrint stderr $ "nextId=" ++ show nextId
       
       let path = storageFilesPath storage ++
                  "/" ++
                  show nextId
       return $ UploadingFile
                  { filePath = path
                  , fileCommit = \name type_ size -> do
                      date <- getPOSIXTime
                      let fileInfo =
                            FileInfo nextId name type_ 0 date size
                      atomically $ do
                        tFileInfo <- newTVar fileInfo
                        modifyTVar' (storageFiles storage) $
                                   Map.insert nextId tFileInfo
                                   
                      hPutStrLn stderr $ "committed"
                      shouldSyncStorage storage
                  , fileDiscard =
                      removeFile path
                  }

allFiles :: Storage -> IO [FileInfo]
allFiles storage =
    atomically $
    readTVar (storageFiles storage) >>=
    mapM (readTVar . snd) . Map.toDescList

getFile :: Integer -> T.Text -> Storage -> IO (Maybe (T.Text, FilePath))
getFile fId fName storage =
    let getFile' = do
          mtFi <- Map.lookup fId <$> 
                  readTVar (storageFiles storage)
          case mtFi of
            Just tFi ->
                do fi <- readTVar tFi
                   case fileName fi of
                     fName' 
                         | fName' == fName ->
                             do writeTVar tFi $
                                          fi { fileDownloads = 
                                                   fileDownloads fi + 1 }
                                return $ 
                                       Just 
                                       ( fileType fi
                                       , storageFilesPath storage ++ "/" ++ show (fileId fi)
                                       )
                     _ ->
                         return Nothing
            _ ->
                return Nothing
    in atomically getFile' <*
       shouldSyncStorage storage
             