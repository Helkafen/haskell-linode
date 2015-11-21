{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Linode.Internal where

import           Control.Error
import           Control.Exception      (IOException, handle)
import           Control.Lens           ((&), (.~), (^?))
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON)
import qualified Data.ByteString.Lazy   as B
--import           Data.Foldable          (traverse_)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
--import           Data.Text.Encoding     (decodeUtf8)
--import qualified Data.Text.IO           as TIO
import qualified Network.Wreq           as W
--import           Network.Wreq.Lens

import           Network.Linode.Parsing
import           Network.Linode.Types


diskTypeToString :: DiskType -> String
diskTypeToString Ext3 = "ext3"
diskTypeToString Ext4 = "ext4"
diskTypeToString Swap = "swap"
diskTypeToString RawDisk = "raw"

paymentTermToInt :: PaymentTerm -> Int
paymentTermToInt OneMonth = 1
paymentTermToInt OneYear = 12
paymentTermToInt TwoYears = 24

getWith :: FromJSON a => W.Options -> ExceptT LinodeError IO a
getWith opts = ExceptT g
  where g = handle (\(e :: IOException) -> return (Left $ NetworkError e)) $ do
              --liftIO $ print (view params opts :: [(T.Text, T.Text)])
              response <- W.getWith opts "https://api.linode.com"
              --liftIO $ traverse_ (TIO.putStrLn . decodeUtf8. B.toStrict) $ response ^? W.responseBody
              return $ parseResponse (fromMaybe B.empty (response ^? W.responseBody))

simpleGetter :: FromJSON a => String -> ApiKey -> ExceptT LinodeError IO a
simpleGetter action apiKey = getWith opts
  where opts = W.defaults & W.param "api_key" .~ [T.pack apiKey]
                          & W.param "api_action" .~ [T.pack action]

maybeOr ::  Monad m => Maybe a -> ExceptT e m a -> ExceptT e m a
maybeOr v p = maybe p return v

fetchAndSelect :: IO (Either LinodeError [a]) -> ([a] -> Maybe a) -> String -> ExceptT LinodeError IO a
fetchAndSelect fetch select name = do
  r <- liftIO fetch
  case r of
    Left e -> throwE $ SelectionError ("Error which fetching a " <> name <> " . " ++ show e)
    Right xs -> case select xs of
      Nothing -> throwE $ SelectionError ("Error: Selection of " <> name <> " returned no value")
      Just x -> return x
