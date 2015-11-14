{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Linode.Internal where

import           Control.Error
import           Control.Exception      (IOException (..), handle)
import           Control.Lens           ((^?))
import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON, decode)
import qualified Data.ByteString.Lazy   as B
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import qualified Data.Text.Encoding     as E
import qualified Data.Text.IO           as TIO
import qualified Network.Wreq           as W

import           Network.Linode.Json
import           Network.Linode.Types


parseResponse :: FromJSON a => B.ByteString -> Either LinodeError a
parseResponse body = case decode body of
  Nothing -> Left err
  Just (Response [] (Just c)) -> Right c
  Just (Response (x:_) _) -> Left x
  otherwise -> Left err
  where err = DeserializationError (E.decodeUtf8 $ B.toStrict body)

get :: FromJSON a => String -> ExceptT LinodeError IO a
get url = ExceptT g
  where g = handle (\(e :: IOException) -> return (Left $ NetworkError e)) $ do
              response <- W.get url
              return $ parseResponse (fromMaybe B.empty (response ^? W.responseBody))

maybeOr ::  Monad m => Maybe a -> ExceptT e m a -> ExceptT e m a
maybeOr v p = maybe p return v

printCreationOptions :: Datacenter -> Plan -> PaymentTerm -> Distribution -> Int -> Int -> IO ()
printCreationOptions datacenter plan paymentTerm distribution rootDiskSize swapSize = do
  TIO.putStrLn $ "Datacenter: " <> datacenterName datacenter
  TIO.putStrLn $  "Plan: " <> planName plan
  putStrLn $  "PaymentTerm: " <> show paymentTerm
  TIO.putStrLn $ "Distribution:" <> distributionName distribution
  putStrLn $ "Disk size" ++ show rootDiskSize
  putStrLn $ "Swap size" ++ show swapSize


fetchAndSelect :: IO (Either LinodeError [a]) -> ([a] -> Maybe a) -> String -> ExceptT LinodeError IO a
fetchAndSelect fetch select name = do
  r <- liftIO fetch
  case r of
    Left e -> throwE $ SelectionError ("Error which fetching a " <> name <> " . " ++ show e)
    Right xs -> case select xs of
      Nothing -> throwE $ SelectionError ("Error: Selection of " <> name <> " returned no value")
      Just x -> return x
