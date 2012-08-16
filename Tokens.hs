module Tokens where

import Control.Applicative
import Control.Concurrent.STM
import qualified Data.HashMap.Strict as HM
import Yesod
import System.Random (randomRIO)
import Control.Monad
import qualified Data.Text as T
import Data.Hashable


newtype Token = Token { unToken :: T.Text }
    deriving (Show, Read, Eq, Ord, Hashable)
tokenLength = 40

instance PathPiece Token where
  fromPathPiece t = Just $ Token t
  toPathPiece = unToken

newtype TokenPool a = TokenPool (TVar (HM.HashMap Token a))


createTokenPool :: IO (TokenPool a)
createTokenPool =
    TokenPool <$>
    newTVarIO HM.empty
    
readToken :: TokenPool a -> Token -> IO (Maybe a)
readToken (TokenPool tMap) token =
    atomically $
    HM.lookup token <$>
    readTVar tMap

newToken :: TokenPool a -> Token -> a -> IO Bool
newToken (TokenPool tMap) token a =
    atomically $ do
      m <- readTVar tMap
      case token `HM.member` m of
        False -> do
          let m' = HM.insert token a m
          writeTVar tMap m'
          return True
        True ->
          return False

deleteToken :: TokenPool a -> Token -> IO ()
deleteToken (TokenPool tMap) token =
    atomically $
    modifyTVar' tMap $
    HM.delete token
