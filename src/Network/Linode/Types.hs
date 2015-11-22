{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Linode.Types where

import           Control.Applicative (optional)
import           Control.Exception   (IOException)
import           Control.Monad       (guard, mzero)
import           Data.Aeson
import           Data.Binary
import qualified Data.Map            as M
import           Data.Maybe          (fromJust, fromMaybe, isJust, mapMaybe)
import           Data.Text
import           GHC.Generics        (Generic)
import           Safe                (readMay)

type ApiKey = String

data LinodeCreationOptions = LinodeCreationOptions {
  datacenterSelect   :: [Datacenter] -> Maybe Datacenter,
  planSelect         :: [Plan] -> Maybe Plan,
  kernelSelect       :: [Kernel] -> Maybe Kernel,
  distributionSelect :: [Distribution] -> Maybe Distribution,
  paymentChoice      :: PaymentTerm,
  swapAmount         :: Int,  -- MB
  password           :: String,
  sshKey             :: Maybe String,
  diskLabel          :: String,
  config             :: Maybe ConfigId
}


newtype ConfigId = ConfigId {unConfigId :: Int}
  deriving (Eq, Show, Generic)

newtype DatacenterId = DatacenterId Int
  deriving (Eq, Ord, Show)

newtype DistributionId = DistributionId Int
  deriving (Eq, Ord, Show)

newtype DiskId = DiskId {unDisk :: Int}
  deriving (Eq, Show)

newtype LinodeId = LinodeId {unLinodeId :: Int}
  deriving (Eq, Ord, Show, Generic)

newtype JobId = JobId Int
  deriving (Eq, Show)

newtype KernelId = KernelId Int
  deriving (Eq, Ord, Show)

newtype PlanId = PlanId Int
  deriving (Eq, Ord, Show)



data DiskType = Ext3 | Ext4 | Swap | RawDisk
  deriving (Eq, Show)

data InstanceStatus = BeingCreated | NewInstance | Running | PoweredOff -- -1: Being Created, 0: Brand New, 1: Running, and 2: Powered Off.
  deriving (Eq, Show)

data PaymentTerm = OneMonth | OneYear | TwoYears
  deriving (Eq, Show)



-- TODO: "ACTIVE_SINCE":"2011-09-23 15:08:13.0",
data AccountInfo = AccountInfo {
  accountTransferPool     :: Int,
  accountTransferUsed     :: Int,
  accountTransferBillable :: Int,
  accountManaged          :: Bool,
  accountBalance          :: Int,
  accountBillingMethod    :: Text
} deriving (Eq, Show)

data Address = Address {
  ip       :: String,
  rdnsName :: String,
  isPublic :: Bool
} deriving (Eq, Show, Generic)

data Datacenter = Datacenter {
  datacenterId       :: DatacenterId,
  datacenterLocation :: Text,
  datacenterName     :: Text
} deriving (Eq, Show)

data Distribution = Distribution {
  distributionId      :: DistributionId,
  distributionName    :: Text,
  is64Bit             :: Bool,
  minImageSize        :: Int,
  requiresPvopsKernel :: Bool -- TODO explain
} deriving (Eq, Show)

{-
TODO missing fields
"WATCHDOG":1,
"LPM_DISPLAYGROUP":"",
"ALERT_BWQUOTA_ENABLED":1,
"ALERT_DISKIO_THRESHOLD":1000,
"BACKUPWINDOW":1,
"ALERT_BWOUT_ENABLED":1,
"ALERT_BWOUT_THRESHOLD":5,
"ALERT_CPU_ENABLED":1,
"ALERT_BWQUOTA_THRESHOLD":80,
"ALERT_BWIN_THRESHOLD":5,
"BACKUPWEEKLYDAY":0,
"ALERT_CPU_THRESHOLD":90,
"ALERT_DISKIO_ENABLED":1,
"ALERT_BWIN_ENABLED":1,
"CREATE_DT":"2015-09-22 11:33:06.0",
"DISTRIBUTIONVENDOR": "Debian",
"ISXEN":0,
"ISKVM":1
-}

{-|
Detailed info about a Linode instance. Memory and transfer are given in MB.
-}
data Instance = Instance {
  instanceId            :: LinodeId, -- "LINODEID":8098,
  instanceName          :: Text, -- LABEL
  instanceDatacenterId  :: DatacenterId, -- "DATACENTERID"
  instancePlanId        :: PlanId, -- "PLANID":1,
  instanceRAM           :: Int, -- "TOTALRAM":1024,
  instanceHD            :: Int, -- "TOTALHD":40960,
  instanceTransfer      :: Int, -- "TOTALXFER":2000,
  instanceBackupEnabled :: Bool, -- "BACKUPSENABLED":1,
  instanceStatus        :: InstanceStatus -- "STATUS" -- :2,
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




data BootedInstance = BootedInstance {
  bootJobId :: JobId
} deriving (Eq, Show)

data CreatedConfig = CreatedConfig {
  createdConfigId :: ConfigId
} deriving (Eq, Show)

data CreatedLinode = CreatedLinode {
  createdLinodeId :: LinodeId
} deriving (Eq, Show)

data CreatedDisk = CreatedDisk {
  diskCreationDiskId :: DiskId,
  diskCreationJobId  :: JobId
} deriving (Eq, Show)

type DeletedLinode = CreatedLinode

data WaitingJob = WaitingJob {
  waitingJobId       :: JobId,
  waitingJobLinodeId :: LinodeId,
  waitingJobSuccess  :: Bool
} deriving (Eq, Show)

{-|
Basic info about a linode instance.
-}
data Linode = Linode {
  linodeId             :: LinodeId,
  linodeConfigId       :: ConfigId,
  linodeDatacenterName :: Text,
  linodePassword       :: String,
  linodeAddresses      :: [Address]
} deriving (Eq, Show, Generic)

instance Binary ConfigId

instance Binary LinodeId

instance Binary Address

instance Binary Linode

type Cluster = [Linode]


data Response a = Response {
  responseErrors  :: [LinodeError],
  responseContent :: Maybe a
} deriving (Eq, Show)


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
                 | DeserializationError Text
                 | NetworkError IOException
                 | UnknownError Int
                 | SelectionError String
  deriving (Eq, Show)


-----------------------------------------------
-- Json instances

instance FromJSON AccountInfo where
  parseJSON (Object v) = AccountInfo <$> v .: "TRANSFER_POOL" <*> v .: "TRANSFER_USED" <*> v .: "TRANSFER_BILLABLE" <*> v .: "MANAGED" <*> v .: "BALANCE" <*> v .: "BILLING_METHOD"
  parseJSON _ = mzero

instance FromJSON BootedInstance where
  parseJSON (Object v) = BootedInstance <$> (JobId <$> v .: "JobID")
  parseJSON _ = mzero

instance FromJSON CreatedLinode where
  parseJSON (Object v) = CreatedLinode <$> (LinodeId <$> v .: "LinodeID")
  parseJSON _ = mzero

instance FromJSON CreatedConfig where
  parseJSON (Object v) = CreatedConfig <$> (ConfigId <$> v .: "ConfigID")
  parseJSON _ = mzero

instance FromJSON CreatedDisk where
  parseJSON (Object v) = CreatedDisk <$> (DiskId <$> v .: "DiskID") <*> (JobId <$> v .: "JobID")
  parseJSON _ = mzero

instance FromJSON Datacenter where
  parseJSON (Object v) = Datacenter <$> (DatacenterId <$> v .: "DATACENTERID") <*> v .: "LOCATION" <*> v .: "ABBR"
  parseJSON _ = mzero

--  TODO: missing: "CREATE_DT":"2007-04-18 00:00:00.0",
instance FromJSON Distribution where
  parseJSON = withObject "distribution" $ \o -> do
    is64 <- o .: "IS64BIT"
    requires <- o .: "REQUIRESPVOPSKERNEL"
    guard (Prelude.all (`elem` [0,1]) [is64,requires])
    Distribution <$> fmap DistributionId (o .: "DISTRIBUTIONID") <*>  o .: "LABEL" <*> pure (isTrue is64) <*> o .: "MINIMAGESIZE" <*> pure (isTrue requires)
      where isTrue = (== (1::Int))

instance FromJSON Instance where
  parseJSON = withObject "instance" $ \o -> do
    s <- o .: "STATUS"
    let status = instanceStatusFromInt s
    guard (isJust status)
    backup <- o .: "BACKUPSENABLED"
    guard (Prelude.all (`elem` [0,1]) [backup])
    Instance <$> fmap LinodeId (o .: "LINODEID")
             <*>  o .: "LABEL"
             <*> fmap DatacenterId (o .: "DATACENTERID")
             <*> fmap PlanId (o .: "PLANID")
             <*> o .: "TOTALRAM"
             <*> o .: "TOTALHD"
             <*> o .: "TOTALXFER"
             <*> pure (isTrue backup)
             <*> pure (fromJust status)
      where isTrue = (== (1::Int))


instance FromJSON Address where
  parseJSON (Object o) = Address <$> o .: "IPADDRESS" <*>  o .: "RDNS_NAME" <*> (isTrue <$> (o .: "ISPUBLIC"))
    where isTrue = (== (1::Int))
  parseJSON _ = mzero


instance FromJSON Kernel where
  parseJSON = withObject "kernel" $ \o -> do
    xen <- o .: "ISXEN"
    kvm <- o .: "ISKVM"
    pvops <- o .: "ISPVOPS"
    guard (Prelude.all (`elem` [0,1]) [xen,kvm,pvops])
    Kernel <$> fmap KernelId (o .: "KERNELID") <*>  o .: "LABEL" <*> pure (isTrue xen) <*> pure (isTrue kvm) <*> pure (isTrue pvops)
      where isTrue = (== (1::Int))

instance FromJSON LinodeError where
  parseJSON = withObject "error response" $ \o -> do
    errorCode <- o .: "ERRORCODE"
    return $ linodeErrorFromCode errorCode

instance FromJSON Plan where
  parseJSON = withObject "plan" $ \o -> do
    d <- o .: "AVAIL"
    let toAvail = M.fromList . mapMaybe (\(k,v) -> may (DatacenterId <$> readMay k,v)) . M.toList
    Plan <$> (PlanId <$> o .: "PLANID") <*>  o .: "LABEL" <*> o .: "RAM" <*> o .: "DISK" <*> o .: "XFER" <*> o .: "HOURLY" <*> pure (toAvail d)
      where may (Nothing, _) = Nothing
            may (Just a, b) = Just (a,b)

instance FromJSON WaitingJob where
  parseJSON = withObject "person" $ \o -> do
    j <- JobId <$> o .: "JOBID"
    i <- LinodeId <$> o .: "LINODEID"
    success :: Maybe Int  <- optional (o .: "HOST_SUCCESS") -- 1 if ok, "" if not
    return $ WaitingJob j i (fromMaybe 0 success == 1)


instance FromJSON a => FromJSON (Response a) where
  parseJSON = withObject "list response" $ \o -> do
    errs <- o .: "ERRORARRAY"
    contentList <- optional (o.: "DATA") -- when ERRORARRAY is not empty, this field is malformed ('{}') and we default it to []
    return $ Response errs contentList



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

instanceStatusFromInt :: Int -> Maybe InstanceStatus
instanceStatusFromInt n = lookup n m
  where m = [(-1, BeingCreated),(0, NewInstance),(1, Running),(2, PoweredOff)]
