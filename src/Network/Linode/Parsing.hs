module Network.Linode.Parsing (
  parseResponse
) where

import           Data.Aeson           (FromJSON, decode)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Encoding   as E

import           Network.Linode.Types

parseResponse :: FromJSON a => B.ByteString -> Either LinodeError a
parseResponse body = case decode body of
  Nothing -> Left e
  Just (Response [] (Just c)) -> Right c
  Just (Response (x:_) _) -> Left x
  _ -> Left e
  where e = DeserializationError (E.decodeUtf8 $ B.toStrict body)
