{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Network.Linode
License     : BSD3
Stability   : experimental

This package contains some helpers to create and configure Linode instances. They all require an API key, which can be created on the Linode website.

Usage example:

> import Network.Linode
> import Data.List (find)
>
> main :: IO()
> main = do
>   apiKey <- fmap (filter (/= '\n')) (readFile "apiKey")
>   let log = True
>   let options = defaultLinodeCreationOptions {
>     datacenterSelect = find ((=="atlanta") . datacenterName),
>     planSelect = find ((=="Linode 2048") . planName),
>   }
>   c <- createLinode apiKey log options
>   print c
-}

module Network.Linode where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async
import           Control.Error
import           Control.Lens
import           Control.Monad            (unless, void)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (decode)
import           Data.List                (find)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T

import           Network.Linode.Internal
import           Network.Linode.Queries
import           Network.Linode.Types

test :: IO ()
test = do
  apiKey <- fmap (filter (/= '\n')) (readFile "apiKey")
  let log = True
  c <- createLinode apiKey log defaultLinodeCreationOptions
  print c


{-|
Create a Linode with everything set up, and boot it.
-}
createLinode :: String -> Bool -> LinodeCreationOptions -> IO (Either LinodeError Linode)
createLinode apiKey log options = do
  i <- runExceptT create
  case i of
    Left e -> return $ Left e
    Right (instId, selected) -> do
      r <- runExceptT $ configure instId options selected
      case r of
        Left e -> runExceptT (deleteInstance apiKey instId) >> return (Left e)
        Right l -> return $ Right l
  where create :: ExceptT LinodeError IO (InstanceId, (Datacenter, Distribution, Plan, Kernel)) = do
          (datacenter, distribution, plan, kernel) <- select apiKey options
          CreatedInstance instId <- createDisklessLinode apiKey (datacenterId datacenter) (planId plan) (paymentChoice options)
          return (instId, (datacenter, distribution, plan, kernel))
        configure instId options (datacenter, distribution, plan, kernel) = do
          let swapSize = swapAmount options
          let rootDiskSize = (1024 * disk plan) - swapSize
          let wait = liftIO (waitUntilCompletion apiKey instId)
          (CreatedDisk diskId diskJobId) <- wait >> createDiskFromDistribution apiKey instId (distributionId distribution) (diskLabel options) rootDiskSize (password options) (sshKey options)
          (CreatedDisk swapId swapJobId) <- wait >> createSwapDisk apiKey instId "swap" swapSize
          (CreatedConfig configId)  <- wait >> maybeOr (CreatedConfig <$> config options) (createConfig apiKey instId (kernelId kernel) "profileLabel" [diskId, swapId])
          (BootedInstance bootJobId) <- wait >> boot apiKey instId configId
          return $ Linode instId configId (datacenterName datacenter) (password options) ""

{-|
Create a Linode cluster with everything set up.
-}
createCluster :: String -> LinodeCreationOptions -> Int -> Bool -> IO (Maybe [Linode])
createCluster apiKey options number log = do
  let optionsList = take number $ map (\(o,i) -> o {diskLabel = diskLabel o <> "-" <> show i}) (zip (repeat options) [0..])
  (errors, linodes) <- partitionEithers <$> mapConcurrently (createLinode apiKey log) optionsList
  case (errors, linodes) of
    ([], xs) -> return $ Just xs
    (es, xs) -> do
      mapM_ print es
      deleteCluster apiKey (map linodeId xs)
      return Nothing

{-|
Default options to create an instance. Please customize the security options.
-}
defaultLinodeCreationOptions :: LinodeCreationOptions
defaultLinodeCreationOptions = LinodeCreationOptions {
  datacenterSelect = find ((=="london") . datacenterName),
  planSelect = find ((=="Linode 1024") . planName),
  kernelSelect = find (("Latest 64 bit" `T.isPrefixOf`) . kernelName),
  distributionSelect = find ((=="Debian 8.1") . distributionName),
  paymentChoice = OneMonth,
  swapAmount = 128,
  password = "We4kP4ssw0rd",
  sshKey = Nothing,
  diskLabel = "haskellMachine",
  config = Nothing
}

{-|
Delete a Linode instance.
-}
deleteInstance :: String -> InstanceId -> ExceptT LinodeError IO DeletedInstance
deleteInstance apiKey inst = get $ deleteInstanceQuery apiKey inst

{-|
Delete a list of Linode instances.
-}
deleteCluster :: String -> [InstanceId] -> IO (Either LinodeError ())
deleteCluster apiKey = runExceptT . mapM_ (deleteInstance apiKey)


{-|
Read your global account information: network usage, billing state and billing method.
-}
getAccountInfo :: String -> ExceptT LinodeError IO AccountInfo
getAccountInfo = get . accountInfoQuery

{-|
Read all Linode datacenters: dallas, fremont, atlanta, newark, london, tokyo, singapore, frankfurt
-}
getDatacenters :: String -> ExceptT LinodeError IO [Datacenter]
getDatacenters = get . datacenterListQuery

{-|
Read all available Linux distributions. For example, Debian 8.1 has id 140.
-}
getDistributions :: String -> ExceptT LinodeError IO [Distribution]
getDistributions = get . distributionListQuery

{-|
Read all your instances.
-}
getInstances :: String -> ExceptT LinodeError IO [Instance]
getInstances = get . linodeListQuery

{-|
Read all available Linux kernels.
-}
getKernels :: String -> ExceptT LinodeError IO [Kernel]
getKernels = get . kernelListQuery

{-|
Read all plans offered by Linode. A plan specifies the available CPU, RAM, network usage and pricing of an instance.
The smallest plan is Linode 1024.
-}
getPlans :: String -> ExceptT LinodeError IO [Plan]
getPlans = get . planListQuery

{-|
Create a Linode Config (a bag of instance options).
-}
createConfig :: String -> InstanceId -> KernelId -> String -> [DiskId] -> ExceptT LinodeError IO CreatedConfig
createConfig apiKey instId kernelId label disksIds = get $ createConfigQuery apiKey instId kernelId label disksIds

{-|
Create a disk from a supported Linux distribution.
-}
createDiskFromDistribution :: String -> InstanceId -> DistributionId -> String -> Int -> String -> Maybe String -> ExceptT LinodeError IO CreatedDisk
createDiskFromDistribution apiKey instanceId distributionId label size pass sshKey = get $ createDistFromDistributionQuery apiKey instanceId distributionId label size pass sshKey

{-|
Create a Linode instance with no disk and no configuration. You probably want createLinode.
-}
createDisklessLinode :: String -> DatacenterId -> PlanId -> PaymentTerm -> ExceptT LinodeError IO CreatedInstance
createDisklessLinode apiKey datacenter plan paymentTerm = get $ createInstanceQuery apiKey datacenter plan paymentTerm

{-|
Create a swap partition.
-}
createSwapDisk :: String -> InstanceId -> String -> Int -> ExceptT LinodeError IO CreatedDisk
createSwapDisk apiKey instanceId label size = get $ createDiskQuery apiKey instanceId label Swap size



{-|
Boot an Linode instance.
-}
boot :: String-> InstanceId -> ConfigId -> ExceptT LinodeError IO BootedInstance
boot apiKey instId configId = get $ bootQuery apiKey instId configId

{-|
Wait until all operations on one instance are finished.
-}
waitUntilCompletion :: String -> InstanceId -> IO()
waitUntilCompletion apiKey instId = do
  waitingJobs <- runExceptT $ get (jobListQuery apiKey instId)
  case waitingJobs of
    Left e -> putStrLn $ "Error during wait:" ++ show e
    Right [] -> return ()
    Right jobs -> unless (all waitingJobSuccess jobs) $ do
        putStrLn "Waiting for job to finish..."
        threadDelay (100*1000)
        waitUntilCompletion apiKey instId


{-|
Select a Datacenter, a Plan, a Linux distribution and kernel from all Linode offering.
-}
select :: String -> LinodeCreationOptions -> ExceptT LinodeError IO (Datacenter, Distribution, Plan, Kernel)
select apiKey options = (,,,) <$>
  fetchAndSelect (runExceptT $ getDatacenters apiKey) (datacenterSelect options) "datacenter" <*>
  fetchAndSelect (runExceptT $ getDistributions apiKey) (distributionSelect options) "distribution" <*>
  fetchAndSelect (runExceptT $ getPlans apiKey) (planSelect options) "plan" <*>
  fetchAndSelect (runExceptT $ getKernels apiKey) (kernelSelect options) "kernel"
