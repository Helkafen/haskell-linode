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
import           Control.Monad            (unless, void, when)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (decode)
import           Data.List                (find)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T
import qualified Network.Wreq             as W

import           Network.Linode.Internal
import           Network.Linode.Types

test :: IO ()
test = do
  apiKey <- fmap (filter (/= '\n')) (readFile "apiKey")
  idrsa <- fmap (filter (/= '\n')) (readFile "idrsa")
  let log = True
  let options = defaultLinodeCreationOptions {
    datacenterSelect = find ((=="atlanta") . datacenterName),
    planSelect = find ((=="Linode 1024") . planName),
    sshKey = Just idrsa
  }
  c <- createLinode apiKey log options
  case c of
    Left e -> print e
    Right l -> print l


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
          printLog "Creating empty linode"
          CreatedInstance instId <- createDisklessLinode apiKey (datacenterId datacenter) (planId plan) (paymentChoice options)
          return (instId, (datacenter, distribution, plan, kernel))
        configure instId options (datacenter, distribution, plan, kernel) = do
          let swapSize = swapAmount options
          let rootDiskSize = (1024 * disk plan) - swapSize
          let wait = liftIO (waitUntilCompletion apiKey instId)
          printLog $ "Creating disk (" ++ show rootDiskSize ++ " MB)"
          (CreatedDisk diskId diskJobId) <- wait >> createDiskFromDistribution apiKey instId (distributionId distribution) (diskLabel options) rootDiskSize (password options) (sshKey options)
          printLog $ "Creating swap (" ++ show swapSize ++ " MB)"
          (CreatedDisk swapId swapJobId) <- wait >> createSwapDisk apiKey instId "swap" swapSize
          printLog "Creating config"
          (CreatedConfig configId)  <- wait >> maybeOr (CreatedConfig <$> config options) (createConfig apiKey instId (kernelId kernel) "profileLabel" [diskId, swapId])
          (BootedInstance bootJobId) <- wait >> boot apiKey instId configId
          addresses <- wait >> getIpList apiKey instId
          printLog $ "Booted linode " ++ show instId ++ "with config " ++ show configId
          return $ Linode instId configId (datacenterName datacenter) (password options) addresses
        printLog l = when log (liftIO $ putStrLn l)


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
deleteInstance apiKey (InstanceId i) = do
  let opts = W.defaults & W.param "LinodeID" .~ [T.pack $ show i]
  getWith opts (query "linode.delete" apiKey)

{-|
Delete a list of Linode instances.
-}
deleteCluster :: String -> [InstanceId] -> IO (Either LinodeError ())
deleteCluster apiKey = runExceptT . mapM_ (deleteInstance apiKey)


{-|
Read your global account information: network usage, billing state and billing method.
-}
getAccountInfo :: String -> ExceptT LinodeError IO AccountInfo
getAccountInfo = noParamQuery "account.info"

{-|
Read all Linode datacenters: dallas, fremont, atlanta, newark, london, tokyo, singapore, frankfurt
-}
getDatacenters :: String -> ExceptT LinodeError IO [Datacenter]
getDatacenters = noParamQuery "avail.datacenters"

{-|
Read all available Linux distributions. For example, Debian 8.1 has id 140.
-}
getDistributions :: String -> ExceptT LinodeError IO [Distribution]
getDistributions = noParamQuery "avail.distributions"

{-|
Read all your instances.
-}
getInstances :: String -> ExceptT LinodeError IO [Instance]
getInstances = noParamQuery "linode.list"

{-|
Read all available Linux kernels.
-}
getKernels :: String -> ExceptT LinodeError IO [Kernel]
getKernels = noParamQuery "avail.kernels"

{-|
Read all plans offered by Linode. A plan specifies the available CPU, RAM, network usage and pricing of an instance.
The smallest plan is Linode 1024.
-}
getPlans :: String -> ExceptT LinodeError IO [Plan]
getPlans = noParamQuery "avail.linodeplans"

{-|
Read all IP addresses of an instance.
-}
getIpList :: String -> InstanceId -> ExceptT LinodeError IO [Address]
getIpList apiKey (InstanceId i) = do
  let opts = W.defaults & W.param "LinodeID" .~ [T.pack $ show i]
  getWith opts (query "linode.ip.list" apiKey)

{-|
Create a Linode Config (a bag of instance options).
-}
createConfig :: String -> InstanceId -> KernelId -> String -> [DiskId] -> ExceptT LinodeError IO CreatedConfig
createConfig apiKey (InstanceId i) (KernelId k) label disksIds = do
  let unDisk (DiskId i) = i
  let disksList = T.intercalate "," $ take 9 $ map (T.pack . show . unDisk) disksIds ++ repeat ""
  let opts = W.defaults & W.param "LinodeID" .~ [T.pack $ show i]
                        & W.param "KernelID" .~ [T.pack $ show k]
                        & W.param "Label" .~ [T.pack label]
                        & W.param "DiskList" .~ [disksList]
                        & W.param "helper_distro" .~ ["true"]
                        & W.param "helper_network" .~ ["true"]
  getWith opts (query "linode.config.create" apiKey)

{-|
Create a disk from a supported Linux distribution.
-}
createDiskFromDistribution :: String -> InstanceId -> DistributionId -> String -> Int -> String -> Maybe String -> ExceptT LinodeError IO CreatedDisk
createDiskFromDistribution apiKey (InstanceId i) (DistributionId d) label size pass sshKey = do
    let opts = W.defaults & W.param "LinodeID" .~ [T.pack $ show i]
                          & W.param "DistributionID" .~ [T.pack $ show d]
                          & W.param "Label" .~ [T.pack label]
                          & W.param "Size" .~ [T.pack $ show size]
                          & W.param "rootPass" .~ [T.pack pass]
                          & case T.pack <$> sshKey of
                              Nothing -> id
                              Just k -> W.param "rootSSHKey" .~ [k]
    getWith opts (query "linode.disk.createfromdistribution" apiKey)

{-|
Create a Linode instance with no disk and no configuration. You probably want createLinode.
-}
createDisklessLinode :: String -> DatacenterId -> PlanId -> PaymentTerm -> ExceptT LinodeError IO CreatedInstance
createDisklessLinode apiKey (DatacenterId d) (PlanId p) paymentTerm = do
  let opts = W.defaults & W.param "DatacenterID" .~ [T.pack $ show d]
                        & W.param "PlanID" .~ [T.pack $ show p]
                        & W.param "PaymentTerm" .~ [T.pack $ show (paymentTermToInt paymentTerm)]
  getWith opts (query "linode.create" apiKey)


{-|
Create a swap partition.
-}
createSwapDisk :: String -> InstanceId -> String -> Int -> ExceptT LinodeError IO CreatedDisk
createSwapDisk apiKey instanceId label = createDisk apiKey instanceId label Swap

{-|
Create a partition.
-}
createDisk :: String -> InstanceId -> String -> DiskType -> Int -> ExceptT LinodeError IO CreatedDisk
createDisk apiKey (InstanceId i) label diskType size = do
  let opts = W.defaults & W.param "LinodeID" .~ [T.pack $ show i]
                        & W.param "Label" .~ [T.pack label]
                        & W.param "Type" .~ [T.pack (diskTypeToString diskType)]
                        & W.param "size" .~ [T.pack $ show size]
  getWith opts (query "linode.disk.create" apiKey)


{-|
Boot an Linode instance.
-}
boot :: String-> InstanceId -> ConfigId -> ExceptT LinodeError IO BootedInstance
boot apiKey (InstanceId i) (ConfigId c) = do
  let opts = W.defaults & W.param "LinodeID" .~ [T.pack $ show i]
                        & W.param "ConfigID" .~ [T.pack $ show c]
  getWith opts (query "linode.boot" apiKey)


{-|
List of pending jobs for this Linode instance.
-}
jobList :: String -> InstanceId -> ExceptT LinodeError IO [WaitingJob]
jobList apiKey (InstanceId i) = do
  let opts = W.defaults & W.param "LinodeID" .~ [T.pack $ show i]
                        & W.param "pendingOnly" .~ ["true"]
  getWith opts (query "linode.job.list" apiKey)

{-|
Wait until all operations on one instance are finished.
-}
waitUntilCompletion :: String -> InstanceId -> IO()
waitUntilCompletion apiKey instId = do
  waitingJobs <- runExceptT $ jobList apiKey instId
  case all waitingJobSuccess <$> waitingJobs of
    Left e -> putStrLn $ "Error during wait:" ++ show e
    Right True -> putStrLn ""
    Right False -> do
        putStr "."
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
