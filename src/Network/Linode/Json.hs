{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Linode.Json where

import           Control.Applicative  (optional)
import           Control.Monad        (guard, mzero)
import           Data.Aeson
import qualified Data.Map             as M
import           Data.Maybe           (fromJust, fromMaybe, isJust, mapMaybe)
import           Network.Linode.Types
import           Safe                 (readMay)


instance FromJSON AccountInfo where
  parseJSON (Object v) = AccountInfo <$> v .: "TRANSFER_POOL" <*> v .: "TRANSFER_USED" <*> v .: "TRANSFER_BILLABLE" <*> v .: "MANAGED" <*> v .: "BALANCE" <*> v .: "BILLING_METHOD"

instance FromJSON BootedInstance where
  parseJSON (Object v) = BootedInstance <$> (JobId <$> v .: "JobID")

instance FromJSON CreatedInstance where
  parseJSON (Object v) = CreatedInstance <$> (InstanceId <$> v .: "LinodeID")

instance FromJSON CreatedConfig where
  parseJSON (Object v) = CreatedConfig <$> (ConfigId <$> v .: "ConfigID")

instance FromJSON CreatedDisk where
  parseJSON (Object v) = CreatedDisk <$> (DiskId <$> v .: "DiskID") <*> (JobId <$> v .: "JobID")

instance FromJSON Datacenter where
  parseJSON (Object v) = Datacenter <$> (DatacenterId <$> v .: "DATACENTERID") <*> v .: "LOCATION" <*> v .: "ABBR"

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
    Instance <$> fmap InstanceId (o .: "LINODEID")
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
  parseJSON (Object v) = Address <$> v .: "IPADDRESS" <*> v .: "RDNS_NAME"


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
      where may (Nothing, x) = Nothing
            may (Just a, b) = Just (a,b)

instance FromJSON WaitingJob where
  parseJSON = withObject "person" $ \o -> do
    j <- JobId <$> o .: "JOBID"
    i <- InstanceId <$> o .: "LINODEID"
    success :: Maybe Int  <- optional (o .: "HOST_SUCCESS") -- 1 if ok, "" if not
    return $ WaitingJob j i (fromMaybe 0 success == 1)


instance FromJSON a => FromJSON (Response a) where
  parseJSON = withObject "list response" $ \o -> do
    errors <- o .: "ERRORARRAY"
    contentList <- optional (o.: "DATA") -- when ERRORARRAY is not empty, this field is malformed ('{}') and we default it to []
    return $ Response errors contentList



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
