module Multipart where

import Data.Conduit
import Network.HTTP.Types
import Control.Applicative
import Data.Conduit.Attoparsec
import Data.Attoparsec hiding (takeTill)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Control.Monad.IO.Class
import qualified Data.CaseInsensitive as CI


data Part = Part
    { partBoundary :: BC.ByteString
    , partHeaders :: RequestHeaders
    } deriving (Show)
    
type PartHandler a = Part -> ResourceT IO (Sink {-L-}BC.ByteString (ResourceT IO) a)

parseMultipart :: PartHandler a -> Sink BC.ByteString (ResourceT IO) [a]
parseMultipart cb =
    do part <- sinkParser parseHeader
       sink <- liftIO $ runResourceT $ cb part
       a <- consumeBody (partBoundary part) sink
       (a:) <$> parseMultipart cb
    
    where parseHeader =
              do boundary <- line
                 headers <- many headerLine
                 endOfLine
                 return $ Part boundary headers 
          line = takeTill (`elem` "\r\n")
                 <*
                 endOfLine
          headerLine = 
              do name <- takeTill (== ':')
                 string ": "
                 value <- takeTill (`elem` "\r\n")
                 endOfLine
                 return (CI.mk name, value)
                                  
          consumeBody boundary sink =