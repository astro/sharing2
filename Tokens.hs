module Tokens where

import Control.Applicative
import Control.Concurrent.STM
import qualified Data.HashMap.Strict as HM
import Yesod
import qualified Data.Text as T
import Data.Hashable


newtype Token = Token { unToken :: T.Text }
    deriving (Show, Read, Eq, Ord, Hashable)

instance PathPiece Token where
  fromPathPiece t = Just $ Token t
  toPathPiece = unToken

newtype TokenPool a = TokenPool (TVar (HM.HashMap Token a))


createTokenPool :: IO (TokenPool a)
createTokenPool =
    TokenPool <$>
    newTVarIO HM.empty
    
readToken :: Token -> TokenPool a -> IO (Maybe a)
readToken token (TokenPool tMap) =
    atomically $
    HM.lookup token <$>
    readTVar tMap
    
writeToken :: Token -> a -> TokenPool a -> IO ()
writeToken token a (TokenPool tMap) =
    atomically $
    modifyTVar' tMap $
    HM.insert token a

newToken :: Token -> a -> TokenPool a -> IO Bool
newToken token a (TokenPool tMap) =
    atomically $ do
      m <- readTVar tMap
      case token `HM.member` m of
        False -> do
          let m' = HM.insert token a m
          writeTVar tMap m'
          return True
        True ->
          return False

deleteToken :: Token -> TokenPool a -> IO ()
deleteToken  token(TokenPool tMap) =
    atomically $
    modifyTVar' tMap $
    HM.delete token

mayDeleteToken :: Token -> (a -> Bool) -> TokenPool a -> IO Bool
mayDeleteToken token f (TokenPool tMap) =
    atomically $ do
      m <- readTVar tMap
      case token `HM.lookup` m of
        Just a | f a -> do
                   -- Predicate f said: delete
                   writeTVar tMap $ HM.delete token m
                   return True
        Just _ ->
            -- Predicate f said: don't delete
            return False
        Nothing ->
            -- Didn't exist at all, deleted
            return True
