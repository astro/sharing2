{-# LANGUAGE ScopedTypeVariables #-}
module Storage where

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
import qualified Control.Exception as E


-- | Model for stored files
data FileInfo = FileInfo 
    { fileId :: Integer
    , fileName :: T.Text
    , fileType :: T.Text
    , fileDownloads :: Integer
    , fileDate :: POSIXTime
    , fileSize :: Integer
    , fileDescription :: Maybe T.Text
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
                         v .: "size" <*>
                         v.:? "description"
  parseJSON _          = empty
  
instance ToJSON FileInfo where
  toJSON (FileInfo fId name type_ downloads date size mDescription) =
         object $
         [ "id" .= fId
         , "name" .= name
         , "type" .= type_
         , "downloads" .= downloads
         , "date" .= (truncate (date * 1000) :: Integer)
         , "size" .= size
         ] ++ 
         maybe [] ((:[]) . ("description" .=)) mDescription

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
  do files <-
         E.catch (unFiles <$>
                  fromMaybe (Files []) <$>
                  decode <$> 
                  LBC.readFile statePath
                 ) (\(_ :: E.IOException) -> 
                        return []
                   )
     files' <-
         filterM (\fi ->
                   print fi >>
                   return True
                 ) files
     filesMap <-
         Map.fromList <$>
         mapM (\fi ->
                   (fileId fi, ) <$> newTVarIO fi
              ) files'
     let nextId = 1 + maximum (0 : map fileId files')
         
     seq nextId $
            Storage <$>
            newTVarIO nextId <*> 
            (pure statePath) <*> 
            (pure filesPath) <*> 
            newTVarIO filesMap <*>
            newTVarIO False <*>
            newTVarIO False


shouldSyncStorage :: Storage -> IO ()
shouldSyncStorage = flip maySyncStorage True

maySyncStorage :: Storage -> Bool -> IO ()
maySyncStorage storage should =
    do doSync <-
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
         _ <- forkIO $ do
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
    { uFileId :: Integer
    , uFilePath :: FilePath
    , uFileCommit :: T.Text -> T.Text -> Integer -> Maybe T.Text -> IO ()
    , uFileDiscard :: IO ()
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
                  { uFileId = nextId
                  , uFilePath = path
                  , uFileCommit = \name type_ size mDescription -> do
                      date <- getPOSIXTime
                      let fileInfo =
                            FileInfo nextId name type_ 0 date size mDescription
                      atomically $ do
                        tFileInfo <- newTVar fileInfo
                        modifyTVar' (storageFiles storage) $
                                   Map.insert nextId tFileInfo
                                   
                      hPutStrLn stderr $ "committed"
                      shouldSyncStorage storage
                  , uFileDiscard =
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
             
setDescription :: Integer -> Maybe T.Text -> Storage -> IO ()
setDescription fId mDescription storage =
    do hPutStrLn stderr $ "set desc: " ++ show mDescription
       atomically $ do 
         mtFi <- Map.lookup fId <$>
                 readTVar (storageFiles storage)
         case mtFi of
           Just tFi ->
               modifyTVar' tFi $ \fi ->
               fi { fileDescription = mDescription }
           Nothing ->
               return ()
       shouldSyncStorage storage
         