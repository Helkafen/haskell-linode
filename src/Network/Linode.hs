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
>   apiKey <- fmap (head . words) (readFile "apiKey")
>   idrsa <- readFile "idrsa"
>   let log = True
>   let options = defaultLinodeCreationOptions {
>     datacenterSelect = find ((=="atlanta") . datacenterName),
>     planSelect = find ((=="Linode 2048") . planName),
>     sshKey = Just idrsa
>   }
>   l <- createLinode apiKey log options
>   print l
>   traverse_ (\a -> waitForSSH a >> setup a) (publicAddress l) -- Setup the instance when the ssh connexion is ready
>
> setup address = P.callCommand $ "scp yourfile root@" <> ip address <> ":/root"

You should see something like this:

Creating empty linode (Linode 2048 at atlanta)
Creating disk (49024 MB)
...................................
Creating swap (128 MB)
...............................
Creating config
.......
Booting
....................................
Booted linode 1481198

And get something like that:

Linode {linodeId = InstanceId {unInstanceId = 1481198}, linodeConfigId = ConfigId {unConfigId = 2251152}, linodeDatacenterName = "atlanta", linodePassword = "We4kP4ssw0rd", linodeAddresses = [Address {ip = "45.79.194.121", rdnsName = "li1293-121.members.linode.com"}]}

-}

module Network.Linode
(
  -- * Most common operations
    createLinode
  , createCluster
  , defaultLinodeCreationOptions
  , waitForSSH
  , deleteInstance
  , deleteCluster

  -- * Lower level API calls
  , getAccountInfo
  , getDatacenters
  , getDistributions
  , getInstances
  , getKernels
  , getPlans
  , getIpList
  , createConfig
  , createDiskFromDistribution
  , createDisklessLinode
  , createSwapDisk
  , createDisk
  , boot
  , jobList

  -- * Helpers
  , waitUntilCompletion
  , select
  , publicAddress

  -- * Examples
  , exampleCreateOneLinode
  , exampleCreateTwoLinodes
  , testOptions
) where

import           Control.Concurrent       (threadDelay)
import qualified Control.Concurrent.Async as A
import           Control.Error hiding (err)
import           Control.Lens
import           Control.Monad            (when)
import           Control.Monad.IO.Class   (liftIO)
import qualified Control.Retry            as R
import           Data.Foldable            (traverse_)
import           Data.List                (find)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T
import qualified Network.Wreq             as W
import           Prelude                  hiding (log)
import qualified System.Process           as P

import           Network.Linode.Internal
import           Network.Linode.Types



{-|
Create a Linode with everything set up, and boot it.
-}
createLinode :: String -> Bool -> LinodeCreationOptions -> IO (Either LinodeError Linode)
createLinode apiKey log options = do
  i <- runExceptT create
  case i of
    Left e -> return $ Left e
    Right (instId, selected) -> do
      r <- runExceptT $ configure instId selected
      case r of
        Left e -> runExceptT (deleteInstance apiKey instId) >> return (Left e)
        Right l -> return $ Right l
  where create :: ExceptT LinodeError IO (InstanceId, (Datacenter, Distribution, Plan, Kernel)) = do
          (datacenter, distribution, plan, kernel) <- select apiKey options
          printLog $ "Creating empty linode (" <> T.unpack (planName plan) <> " at " <> T.unpack (datacenterName datacenter) <> ")"
          CreatedInstance instId <- createDisklessLinode apiKey (datacenterId datacenter) (planId plan) (paymentChoice options)
          return (instId, (datacenter, distribution, plan, kernel))
        configure instId (datacenter, distribution, plan, kernel) = do
          let swapSize = swapAmount options
          let rootDiskSize = (1024 * disk plan) - swapSize
          let wait = liftIO (waitUntilCompletion apiKey instId)
          printLog $ "Creating disk (" ++ show rootDiskSize ++ " MB)"
          (CreatedDisk diskId _) <- wait >> createDiskFromDistribution apiKey instId (distributionId distribution) (diskLabel options) rootDiskSize (password options) (sshKey options)
          printLog $ "Creating swap (" ++ show swapSize ++ " MB)"
          (CreatedDisk swapId _) <- wait >> createSwapDisk apiKey instId "swap" swapSize
          printLog "Creating config"
          (CreatedConfig configId)  <- wait >> maybeOr (CreatedConfig <$> config options) (createConfig apiKey instId (kernelId kernel) "profile" [diskId, swapId])
          printLog "Booting"
          (BootedInstance _) <- wait >> boot apiKey instId configId
          printLog "Still booting"
          addresses <- wait >> getIpList apiKey instId
          printLog $ "Booted linode " ++ show (unInstanceId instId)
          return $ Linode instId configId (datacenterName datacenter) (password options) addresses
        printLog l = when log (liftIO $ putStrLn l)


{-|
Create a Linode cluster with everything set up.
-}
createCluster :: String -> LinodeCreationOptions -> Int -> Bool -> IO (Either [LinodeError] [Linode])
createCluster apiKey options number log = do
  let optionsList = take number $ map (\(o,i) -> o {diskLabel = diskLabel o <> "-" <> show i}) (zip (repeat options) ([0..] :: [Int]))
  r <- partitionEithers <$> A.mapConcurrently (createLinode apiKey log) optionsList
  case r of
    ([], linodes) -> return (Right linodes)
    (errors, linodes) -> do
      _ <- deleteCluster apiKey (map linodeId linodes)
      return (Left errors)

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
  diskLabel = "mainDisk",
  config = Nothing
}

-- TODO: only works in linux and macos
{-|
Wait until an ssh connexion is possible, and add the Linode instance in ssh's known_hosts list.
-}
waitForSSH :: Address -> IO ()
waitForSSH address = R.recoverAll retryPolicy $ P.callCommand $ "ssh -q -o StrictHostKeyChecking=no root@" <> ip address <> " exit"
  where retryPolicy = R.constantDelay oneSecond <> R.limitRetries 100
        oneSecond = 1000 * 1000


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
createDiskFromDistribution apiKey (InstanceId i) (DistributionId d) label size pass idrsa = do
    let opts = W.defaults & W.param "LinodeID" .~ [T.pack $ show i]
                          & W.param "DistributionID" .~ [T.pack $ show d]
                          & W.param "Label" .~ [T.pack label]
                          & W.param "Size" .~ [T.pack $ show size]
                          & W.param "rootPass" .~ [T.pack pass]
                          & case T.pack <$> idrsa of
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
createSwapDisk apiKey instId label = createDisk apiKey instId label Swap

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


{-|
Pick one public address of the Linode Instance
-}
publicAddress :: Linode -> Maybe Address
publicAddress = headMay . linodeAddresses

{-|
Example of Linode creation.
-}
exampleCreateOneLinode :: IO (Maybe Linode)
exampleCreateOneLinode = do
  (apiKey, options) <- testOptions
  c <- createLinode apiKey True options
  case c of
    Left err -> do
      print err
      return Nothing
    Right linode -> do
      traverse_ (\a -> waitForSSH a >> setup a) (publicAddress linode)
      return (Just linode)
  where setup address = P.callCommand $ "scp TODO root@" <> ip address <> ":/root"

{-|
Example of Linodes creation.
-}
exampleCreateTwoLinodes :: IO (Maybe [Linode])
exampleCreateTwoLinodes = do
  (apiKey, options) <- testOptions
  c <- createCluster apiKey options 2 True
  case c of
    Left errors -> do
      print ("error(s) in cluster creation" ++ show errors)
      return Nothing
    Right linodes -> do
      mapM_ (traverse_ (\a -> waitForSSH a >> setup a) . publicAddress) linodes
      return (Just linodes)
  where setup address = P.callCommand $ "scp TODO root@" <> ip address <> ":/root"

{-|
A set of options for the examples. It expects the apiKey and idrsa files in the current directory.
-}
testOptions :: IO (String, LinodeCreationOptions)
testOptions = do
  apiKey <- fmap (head . words) (readFile "apiKey")
  idrsa <- readFile "idrsa"
  return (apiKey, defaultLinodeCreationOptions {
    datacenterSelect = find ((=="atlanta") . datacenterName),
    planSelect = find ((=="Linode 1024") . planName),
    sshKey = Just idrsa
  })
