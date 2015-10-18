{-# LANGUAGE DeriveGeneric #-}

module Network.Linode.Types where

import           Control.Exception (IOException (..))
import           Data.Binary
import qualified Data.Map          as M
import           Data.Text
import           GHC.Generics      (Generic)


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


newtype ConfigId = ConfigId Int
  deriving (Eq, Show, Read, Generic)

newtype DatacenterId = DatacenterId Int
  deriving (Eq, Ord, Read, Show)

newtype DistributionId = DistributionId Int
  deriving (Eq, Ord, Show)

newtype DiskId = DiskId Int
  deriving (Eq, Show)

newtype InstanceId = InstanceId Int
  deriving (Eq, Ord, Show, Read, Generic)

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

data Datacenter = Datacenter {
  datacenterId       :: DatacenterId,
  datacenterLocation :: Text,
  datacenterName     :: Text
} deriving (Eq, Show, Read)

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

data Instance = Instance {
  instanceId            :: InstanceId, -- "LINODEID":8098,
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

data CreatedInstance = CreatedInstance {
  createdInstanceId :: InstanceId
} deriving (Eq, Show)

data CreatedDisk = CreatedDisk {
  diskCreationDiskId :: DiskId,
  diskCreationJobId  :: JobId
} deriving (Eq, Show)

type DeletedInstance = CreatedInstance

data WaitingJob = WaitingJob {
  waitingJobId         :: JobId,
  waitingJobInstanceId :: InstanceId,
  waitingJobSuccess    :: Bool
} deriving (Eq, Show)


data Linode = Linode {
  linodeId             :: InstanceId,
  linodeConfigId       :: ConfigId,
  linodeDatacenterName :: Text,
  linodePassword       :: String,
  linodeIpAddress      :: String
} deriving (Eq, Show, Read, Generic)

instance Binary ConfigId

instance Binary InstanceId

instance Binary Linode

type Cluster = [Linode]


data Response a = Response {
  errors  :: [LinodeError],
  content :: Maybe a
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
