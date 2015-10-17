{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Linode where

import           Control.Applicative  (optional)
import           Control.Monad        (guard)
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe, mapMaybe)
import           Data.Monoid          ((<>))
import           Data.Text
import           Network.Wreq
import           Safe                 (readMay)


newtype DatacenterId = DatacenterId Int
  deriving (Eq, Ord, Show)

newtype DistributionId = DistributionId Int
  deriving (Eq, Ord, Show)

newtype KernelId = KernelId Int
  deriving (Eq, Ord, Show)

newtype PlanId = PlanId Int
  deriving (Eq, Ord, Show)

data Datacenter = Datacenter {
  datacenterId       :: DatacenterId,
  datacenterLocation :: Text,
  datacenterAbbr     :: Text
} deriving (Eq, Show)

data Distribution = Distribution {
  distributionId      :: DistributionId,
  distributionName    :: Text,
  is64Bit             :: Bool,
  minImageSize        :: Int,
  requiresPvopsKernel :: Bool -- TODO explain
} deriving (Eq, Show)

data Kernel = Kernel {
  kernelId   :: KernelId,
  kernelName :: Text,
  isXen      :: Bool,
  isKVM      :: Bool,
  isPVOPS    :: Bool
} deriving (Eq, Show)

data Plan = Plan {
  planId         :: PlanId,
  planName       :: Text,
  ram            :: Int,
  disk           :: Int,
  xfer           :: Int,
  hourly         :: Double,
  availabilities :: M.Map DatacenterId Int
} deriving (Eq, Show)

-- TODO: "ACTIVE_SINCE":"2011-09-23 15:08:13.0",
data AccountInfo = AccountInfo {
  accountTransferPool     :: Int,
  accountTransferUsed     :: Int,
  accountTransferBillable :: Int,
  accountManaged          :: Bool,
  accountBalance          :: Int,
  accountBillingMethod    :: Text
} deriving (Eq, Show)

instance FromJSON Plan where
  parseJSON = withObject "plan" $ \o -> do
    d <- o .: "AVAIL"
    let toAvail = M.fromList . mapMaybe (\(k,v) -> may (DatacenterId <$> readMay k,v)) . M.toList
    Plan <$> fmap PlanId (o .: "PLANID") <*>  o .: "LABEL" <*> o .: "RAM" <*> o .: "DISK" <*> o .: "XFER" <*> o .: "HOURLY" <*> pure (toAvail d)
      where may (Nothing, x) = Nothing
            may (Just a, b) = Just (a,b)

data LinodeRequest = RequestDatacenterList | RequestDistributionList | RequestKernelList | RequestPlanList | RequestAccountInfo

instance FromJSON Datacenter where
  parseJSON (Object v) = Datacenter <$> fmap DatacenterId (v .: "DATACENTERID") <*> v .: "LOCATION" <*> v .: "ABBR"

--  TODO: missing: "CREATE_DT":"2007-04-18 00:00:00.0",
instance FromJSON Distribution where
  parseJSON = withObject "distribution" $ \o -> do
    is64 <- o .: "IS64BIT"
    requires <- o .: "REQUIRESPVOPSKERNEL"
    guard (Prelude.all (`elem` [0,1]) [is64,requires])
    Distribution <$> fmap DistributionId (o .: "DISTRIBUTIONID") <*>  o .: "LABEL" <*> pure (isTrue is64) <*> o .: "MINIMAGESIZE" <*> pure (isTrue requires)
      where isTrue = (== (1::Int))

instance FromJSON Kernel where
  parseJSON = withObject "kernel" $ \o -> do
    xen <- o .: "ISXEN"
    kvm <- o .: "ISKVM"
    pvops <- o .: "ISPVOPS"
    guard (Prelude.all (`elem` [0,1]) [xen,kvm,pvops])
    Kernel <$> fmap KernelId (o .: "KERNELID") <*>  o .: "LABEL" <*> pure (isTrue xen) <*> pure (isTrue kvm) <*> pure (isTrue pvops)
      where isTrue = (== (1::Int))

instance FromJSON AccountInfo where
  parseJSON (Object v) = AccountInfo <$> v .: "TRANSFER_POOL" <*> v .: "TRANSFER_USED" <*> v .: "TRANSFER_BILLABLE" <*> v .: "MANAGED" <*> v .: "BALANCE" <*> v .: "BILLING_METHOD"

instance FromJSON LinodeError where
  parseJSON = withObject "error response" $ \o -> do
    errorCode <- o .: "ERRORCODE"
    return $ linodeErrorFromCode errorCode

data LinodeError = BadRequest
                 | NoActionWasRequested
                 | TheRequestedClassDoesNotExist
                 | AuthenticationFailed
                 | ObjectNotFound
                 | ARequiredPropertyIsMissingForThisAction
                 | PropertyIsInvalid
                 | ADataValidationErrorHasOccurred
                 | MethodNotImplemented
                 | TooManyBatchedRequests
                 | RequestArrayIsntValidJSONOrWDDX
                 | BatchApproachingTimeout
                 | PermissionDenied
                 | APIRateLimitExceeded
                 | ChargingTheCreditCardFailed
                 | CreditCardIsExpired
                 | LimitOfLinodesAddedPerHourReached
                 | LinodeMustHaveNoDisksBeforeDelete
                 | DeserializationError
                 | UnknownError Int
  deriving (Eq, Show)

data ListResponse a = ListResponse {
  errors  :: [LinodeError],
  content :: [a]
} deriving (Eq, Show)

data ObjectResponse a = ObjectResponse {
  oErrors  :: [LinodeError],
  oContent :: Maybe a
} deriving (Eq, Show)

instance FromJSON a => FromJSON (ListResponse a) where
  parseJSON = withObject "list response" $ \o -> do
    errors <- o .: "ERRORARRAY"
    contentList <- optional (o.: "DATA") -- when ERRORARRAY is not empty, this field is malformed ('{}') and we default it to []
    return $ ListResponse errors (fromMaybe [] contentList)

instance FromJSON a => FromJSON (ObjectResponse a) where
  parseJSON = withObject "object response" $ \o -> do
    errors <- o .: "ERRORARRAY"
    object <- optional (o.: "DATA") -- when ERRORARRAY is not empty, this field is malformed ('{}') and we default it to []
    return $ ObjectResponse errors object

parseAccountInfo :: B.ByteString -> Either LinodeError AccountInfo
parseAccountInfo body = case decode body of
  Nothing -> Left DeserializationError
  Just (ObjectResponse [] (Just c)) -> Right c
  Just (ObjectResponse [] Nothing) -> Left DeserializationError
  Just (ObjectResponse (x:_) _) -> Left x

parseListResponse :: FromJSON a => B.ByteString -> Either LinodeError [a]
parseListResponse body = case decode body of
  Nothing -> Left DeserializationError
  Just (ListResponse [] c) -> Right c
  Just (ListResponse (x:_) _) -> Left x



linodeErrorFromCode :: Int -> LinodeError
linodeErrorFromCode 1 = BadRequest
linodeErrorFromCode 2 = NoActionWasRequested
linodeErrorFromCode 3 = TheRequestedClassDoesNotExist
linodeErrorFromCode 4 = AuthenticationFailed
linodeErrorFromCode 5 = ObjectNotFound
linodeErrorFromCode 6 = ARequiredPropertyIsMissingForThisAction
linodeErrorFromCode 7 = PropertyIsInvalid
linodeErrorFromCode 8 = ADataValidationErrorHasOccurred
linodeErrorFromCode 9 = MethodNotImplemented
linodeErrorFromCode 10 = TooManyBatchedRequests
linodeErrorFromCode 11 = RequestArrayIsntValidJSONOrWDDX
linodeErrorFromCode 12 = BatchApproachingTimeout
linodeErrorFromCode 13 = PermissionDenied
linodeErrorFromCode 14 = APIRateLimitExceeded
linodeErrorFromCode 30 = ChargingTheCreditCardFailed
linodeErrorFromCode 31 = CreditCardIsExpired
linodeErrorFromCode 40 = LimitOfLinodesAddedPerHourReached
linodeErrorFromCode 41 = LinodeMustHaveNoDisksBeforeDelete
linodeErrorFromCode x = UnknownError x

url :: LinodeRequest -> Text -> Text
url RequestDatacenterList apiKey = "https://api.linode.com/?api_key=" <> apiKey <> "&api_action=avail.datacenters"
url RequestDistributionList apiKey = "https://api.linode.com/?api_key=" <> apiKey <> "&api_action=avail.distributions"
url RequestKernelList apiKey = "https://api.linode.com/?api_key=" <> apiKey <> "&api_action=avail.kernels"
url RequestPlanList apiKey = "https://api.linode.com/?api_key=" <> apiKey <> "&api_action=avail.linodeplans"
url RequestAccountInfo apiKey = "https://api.linode.com/?api_key=" <> apiKey <> "&api_action=account.info"

someFunc :: IO ()
someFunc = do
  let p = decode "{\"ERRORARRAY\":[{\"ERRORCODE\":4,\"ERRORMESSAGE\":\"Authentication failed\"}],\"DATA\":[],\"ACTION\":\"avail.datacenters\"}" :: Maybe (ListResponse Datacenter)
  print p
  let q = decode "{\"ERRORARRAY\":[{\"ERRORCODE\":4,\"ERRORMESSAGE\":\"Authentication failed\"}],\"DATA\":{},\"ACTION\":\"avail.datacenters\"}" :: Maybe (ListResponse Datacenter)
  print q
  let p = decode "{\"LOCATION\":\"Dallas, TX, USA\",\"DATACENTERID\":2,\"ABBR\":\"dallas\"}" :: Maybe Datacenter
  print p
  let p2 = decode "{\"ERRORARRAY\":[],\"DATA\":[{\"LOCATION\":\"Dallas, TX, USA\",\"DATACENTERID\":2,\"ABBR\":\"dallas\"},{\"LOCATION\":\"Fremont, CA, USA\",\"DATACENTERID\":3,\"ABBR\":\"fremont\"},{\"LOCATION\":\"Atlanta, GA, USA\",\"DATACENTERID\":4,\"ABBR\":\"atlanta\"},{\"LOCATION\":\"Newark, NJ, USA\",\"DATACENTERID\":6,\"ABBR\":\"newark\"},{\"LOCATION\":\"London, England, UK\",\"DATACENTERID\":7,\"ABBR\":\"london\"},{\"LOCATION\":\"Tokyo, JP\",\"DATACENTERID\":8,\"ABBR\":\"tokyo\"},{\"LOCATION\":\"Singapore, SG\",\"DATACENTERID\":9,\"ABBR\":\"singapore\"},{\"LOCATION\":\"Frankfurt, DE\",\"DATACENTERID\":10,\"ABBR\":\"frankfurt\"}],\"ACTION\":\"avail.datacenters\"}" :: Maybe (ListResponse Datacenter)
  print p2
  let p3 = decode "{\"REQUIRESPVOPSKERNEL\":0,\"DISTRIBUTIONID\":142,\"IS64BIT\":1,\"LABEL\":\"Arch Linux 2015.08\",\"MINIMAGESIZE\":800,\"CREATE_DT\":\"2015-08-24 11:17:18.0\"}" :: Maybe Distribution
  print p3
  let p4 = decode "{\"ISKVM\":1,\"LABEL\":\"Latest 32 bit (4.1.5-x86-linode80)\",\"ISXEN\":1,\"ISPVOPS\":1,\"KERNELID\":137}" :: Maybe Kernel
  print p4
  let p5 = decode "{\"CORES\":1,\"PRICE\":10.00,\"RAM\":1024,\"XFER\":2000,\"PLANID\":1,\"LABEL\":\"Linode 1024\",\"AVAIL\":{\"3\":500,\"2\":500,\"10\":500,\"7\":500,\"6\":500,\"4\":500,\"9\":500,\"8\":500},\"DISK\":24,\"HOURLY\":0.0150}" :: Maybe Plan
  print p5
  let p6 = decode "{\"TRANSFER_USED\":1,\"BALANCE\":0.0000,\"TRANSFER_BILLABLE\":0,\"BILLING_METHOD\":\"prepay\",\"TRANSFER_POOL\":2000,\"ACTIVE_SINCE\":\"2011-03-10 19:18:43.0\",\"MANAGED\":false}" :: Maybe AccountInfo
  print p6
  --dcs <- datacenterList apiKey
  --print dcs
  return ()
