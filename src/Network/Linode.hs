{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Network.Linode
License     : BSD3
Stability   : experimental

This package contains some helpers to create and configure <https://www.linode.com/ Linode> instances. They all require an API key, which can be created on the Linode website.

Usage example. We want to create one Linode instance in Atlanta with 1GB of RAM:

> import Network.Linode
> import Data.List (find)
> import qualified System.Process as P
> import Data.Foldable (traverse_)
> import Data.Monoid ((<>))
>
> main :: IO()
> main = do
>   apiKey <- fmap (head . words) (readFile "apiKey")
>   sshPublicKey <- readFile "id_rsa.pub"
>   let options = defaultLinodeCreationOptions {
>     datacenterChoice = "atlanta",
>     planChoice = "Linode 1024",
>     sshKey = Just sshPublicKey
>   }
>   c <- createLinode apiKey True options
>   case c of
>     Left err -> print err
>     Right linode -> do
>       traverse_ (\a -> waitForSSH a >> setup a) (publicAddress linode)
>       print linode
>
> setup address = P.callCommand $ "scp yourfile root@" <> ip address <> ":/root"

You should see something like this:

> Creating empty linode (Linode 1024 at atlanta)
> Creating disk (24448 MB)
> ..............
> Creating swap (128 MB)
> ........
> Creating config
> Booting
> ......................................
> Booted linode 1481198

And get something like that:

> Linode {
>   linodeId = LinodeId {unLinodeId = 1481198},
>   linodeConfigId = ConfigId {unConfigId = 2251152},
>   linodeDatacenterName = "atlanta",
>   linodePassword = "We4kP4ssw0rd",
>   linodeAddresses = [Address {ip = "45.79.194.121", rdnsName = "li1293-121.members.linode.com"}]}

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

  , module Network.Linode.Types
) where

import           Control.Concurrent       (threadDelay)
import qualified Control.Concurrent.Async as A
import           Control.Error            hiding (err)
import           Control.Lens
import           Control.Monad            (when)
import           Control.Monad.IO.Class   (liftIO)
import qualified Control.Retry            as R
import           Data.Foldable            (traverse_)
import           Data.List                (find, sortBy)
import           Data.Monoid              ((<>))
import           Data.Ord                 (comparing)
import qualified Data.Text                as T
import qualified Network.Wreq             as W
import           Prelude                  hiding (log)
import qualified System.Process           as P

import           Network.Linode.Internal
import           Network.Linode.Types



{-|
Create a Linode instance and boot it.
-}
createLinode :: ApiKey -> Bool -> LinodeCreationOptions -> IO (Either LinodeError Linode)
createLinode apiKey log options = do
  i <- runExceptT create
  case i of
    Left e -> return $ Left e
    Right (linId, selected) -> do
      r <- runExceptT $ configure linId selected
      case r of
        Left e ->  deleteInstance apiKey linId >> return (Left e)
        Right l -> return $ Right l
  where create :: ExceptT LinodeError IO (LinodeId, (Datacenter, Distribution, Plan, Kernel)) = do
          (datacenter, distribution, plan, kernel) <- select apiKey options
          printLog $ "Creating empty linode (" <> T.unpack (planName plan) <> " at " <> T.unpack (datacenterName datacenter) <> ")"
          CreatedLinode linId <- createDisklessLinode apiKey (datacenterId datacenter) (planId plan) (paymentChoice options)
          return (linId, (datacenter, distribution, plan, kernel))
        configure linId (datacenter, distribution, plan, kernel) = do
          let swapSize = swapAmount options
          let rootDiskSize = (1024 * disk plan) - swapSize
          let wait = liftIO (waitUntilCompletion apiKey linId log)
          (CreatedDisk diskId _) <- createDiskFromDistribution apiKey linId (distributionId distribution) (diskLabel options) rootDiskSize (password options) (sshKey options)
          printLog ("Creating disk (" ++ show rootDiskSize ++ " MB)") >> wait
          (CreatedDisk swapId _) <- createSwapDisk apiKey linId "swap" swapSize
          printLog ("Creating swap (" ++ show swapSize ++ " MB)") >> wait
          (CreatedConfig configId)  <- maybeOr (CreatedConfig <$> config options) (createConfig apiKey linId (kernelId kernel) "profile" [diskId, swapId])
          printLog "Creating config"
          (BootedInstance _) <- boot apiKey linId configId
          printLog "Booting" >> wait
          addresses <- getIpList apiKey linId
          printLog $ "Booted linode " ++ show (unLinodeId linId)
          return $ Linode linId configId (datacenterName datacenter) (password options) addresses
        printLog l = when log (liftIO $ putStrLn l)


{-|
Create a Linode cluster.
-}
createCluster :: ApiKey -> LinodeCreationOptions -> Int -> Bool -> IO (Either [LinodeError] [Linode])
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
  datacenterChoice = "london",
  planChoice = "Linode 1024",
  kernelSelect = find (("Latest 64 bit" `T.isPrefixOf`) . kernelName), -- Most recent 64 bits kernel
  distributionSelect = lastMay . sortBy (comparing distributionName) . filter ((T.pack "Debian" `T.isPrefixOf`) . distributionName), -- Most recent Debian
  paymentChoice = OneMonth,
  swapAmount = 128,
  password = "We4kP4ssw0rd",
  sshKey = Nothing,
  diskLabel = "mainDisk",
  config = Nothing
}

-- TODO: only works in linux and macos
{-|
Wait until an ssh connexion is possible, then add the Linode's ip in known_hosts.

A newly created Linode is unreachable during a few seconds.
-}
waitForSSH :: Address -> IO ()
waitForSSH address = R.recoverAll retryPolicy command
  where retryPolicy = R.constantDelay oneSecond <> R.limitRetries 100
        oneSecond = 1000 * 1000
        command _ = P.callCommand $ "ssh -q -o StrictHostKeyChecking=no root@" <> ip address <> " exit"


{-|
Delete a Linode instance.
-}
deleteInstance :: ApiKey -> LinodeId -> IO (Either LinodeError DeletedLinode)
deleteInstance apiKey (LinodeId i) = runExceptT $ getWith $
  W.defaults & W.param "api_key" .~ [T.pack apiKey]
             & W.param "api_action" .~ [T.pack "linode.delete"]
             & W.param "LinodeID" .~ [T.pack $ show i]
             & W.param "skipChecks" .~ ["true"]

{-|
Delete a list of Linode instances.
-}
deleteCluster :: ApiKey -> [LinodeId] -> IO ([LinodeError],[DeletedLinode])
deleteCluster apiKey linodes = partitionEithers <$> mapM (deleteInstance apiKey) linodes


{-|
Read your global account information: network usage, billing state and billing method.
-}
getAccountInfo :: ApiKey -> ExceptT LinodeError IO AccountInfo
getAccountInfo = simpleGetter "account.info"

{-|
Read all Linode datacenters: dallas, fremont, atlanta, newark, london, tokyo, singapore, frankfurt
-}
getDatacenters :: ApiKey -> ExceptT LinodeError IO [Datacenter]
getDatacenters = simpleGetter "avail.datacenters"

{-|
Read all available Linux distributions. For example, Debian 8.1 has id 140.
-}
getDistributions :: ApiKey -> ExceptT LinodeError IO [Distribution]
getDistributions = simpleGetter "avail.distributions"

{-|
Read detailed information about all your instances.
-}
getInstances :: ApiKey -> ExceptT LinodeError IO [Instance]
getInstances = simpleGetter "linode.list"

{-|
Read all available Linux kernels.
-}
getKernels :: ApiKey -> ExceptT LinodeError IO [Kernel]
getKernels = simpleGetter "avail.kernels"

{-|
Read all plans offered by Linode. A plan specifies the available CPU, RAM, network usage and pricing of an instance.
The smallest plan is Linode 1024.
-}
getPlans :: ApiKey -> ExceptT LinodeError IO [Plan]
getPlans = simpleGetter "avail.linodeplans"

{-|
Read all IP addresses of an instance.
-}
getIpList :: ApiKey -> LinodeId -> ExceptT LinodeError IO [Address]
getIpList apiKey (LinodeId i) = getWith $
  W.defaults & W.param "api_key" .~ [T.pack apiKey]
             & W.param "api_action" .~ [T.pack "linode.ip.list"]
             & W.param "LinodeID" .~ [T.pack $ show i]

{-|
Create a Linode Config (a bag of instance options).
-}
createConfig :: ApiKey -> LinodeId -> KernelId -> String -> [DiskId] -> ExceptT LinodeError IO CreatedConfig
createConfig apiKey (LinodeId i) (KernelId k) label disksIds = do
  let disksList = T.intercalate "," $ take 9 $ map (T.pack . show . unDisk) disksIds ++ repeat ""
  let opts = W.defaults & W.param "api_key" .~ [T.pack apiKey]
                        & W.param "api_action" .~ [T.pack "linode.config.create"]
                        & W.param "LinodeID" .~ [T.pack $ show i]
                        & W.param "KernelID" .~ [T.pack $ show k]
                        & W.param "Label" .~ [T.pack label]
                        & W.param "DiskList" .~ [disksList]
                        & W.param "helper_distro" .~ ["true"]
                        & W.param "helper_network" .~ ["true"]
  getWith opts

{-|
Create a disk from a supported Linux distribution. Size in MB.
-}
createDiskFromDistribution :: ApiKey -> LinodeId -> DistributionId -> String -> Int -> String -> Maybe String -> ExceptT LinodeError IO CreatedDisk
createDiskFromDistribution apiKey (LinodeId i) (DistributionId d) label size pass sshPublicKey = getWith $
    W.defaults & W.param "api_key" .~ [T.pack apiKey]
               & W.param "api_action" .~ [T.pack "linode.disk.createfromdistribution"]
               & W.param "LinodeID" .~ [T.pack $ show i]
               & W.param "DistributionID" .~ [T.pack $ show d]
               & W.param "Label" .~ [T.pack label]
               & W.param "Size" .~ [T.pack $ show size]
               & W.param "rootPass" .~ [T.pack pass]
               & case T.pack <$> sshPublicKey of
                   Nothing -> id
                   Just k -> W.param "rootSSHKey" .~ [k]

{-|
Create a Linode instance with no disk and no configuration. You probably want createLinode instead.
-}
createDisklessLinode :: ApiKey -> DatacenterId -> PlanId -> PaymentTerm -> ExceptT LinodeError IO CreatedLinode
createDisklessLinode apiKey (DatacenterId d) (PlanId p) paymentTerm = getWith $
  W.defaults & W.param "api_key" .~ [T.pack apiKey]
             & W.param "api_action" .~ [T.pack "linode.create"]
             & W.param "DatacenterID" .~ [T.pack $ show d]
             & W.param "PlanID" .~ [T.pack $ show p]
             & W.param "PaymentTerm" .~ [T.pack $ show (paymentTermToInt paymentTerm)]

{-|
Create a swap partition.
-}
createSwapDisk :: ApiKey -> LinodeId -> String -> Int -> ExceptT LinodeError IO CreatedDisk
createSwapDisk apiKey linId label = createDisk apiKey linId label Swap

{-|
Create a partition.
-}
createDisk :: ApiKey -> LinodeId -> String -> DiskType -> Int -> ExceptT LinodeError IO CreatedDisk
createDisk apiKey (LinodeId i) label diskType size = getWith $
  W.defaults & W.param "api_key" .~ [T.pack apiKey]
             & W.param "api_action" .~ [T.pack "linode.disk.create"]
             & W.param "LinodeID" .~ [T.pack $ show i]
             & W.param "Label" .~ [T.pack label]
             & W.param "Type" .~ [T.pack (diskTypeToString diskType)]
             & W.param "size" .~ [T.pack $ show size]


{-|
Boot a Linode instance.
-}
boot :: ApiKey-> LinodeId -> ConfigId -> ExceptT LinodeError IO BootedInstance
boot apiKey (LinodeId i) (ConfigId c) = getWith $
  W.defaults & W.param "api_key" .~ [T.pack apiKey]
             & W.param "api_action" .~ [T.pack "linode.boot"]
             & W.param "LinodeID" .~ [T.pack $ show i]
             & W.param "ConfigID" .~ [T.pack $ show c]

{-|
List of pending jobs for this Linode instance.
-}
jobList :: ApiKey -> LinodeId -> ExceptT LinodeError IO [WaitingJob]
jobList apiKey (LinodeId i) = getWith $
  W.defaults & W.param "api_key" .~ [T.pack apiKey]
             & W.param "api_action" .~ [T.pack "linode.job.list"]
             & W.param "LinodeID" .~ [T.pack $ show i]
             & W.param "pendingOnly" .~ ["true"]

{-|
Wait until all operations on one instance are done.
-}
waitUntilCompletion :: ApiKey -> LinodeId -> Bool -> IO()
waitUntilCompletion apiKey linId log = do
  waitingJobs <- runExceptT $ jobList apiKey linId
  case all waitingJobSuccess <$> waitingJobs of
    Left e -> putStrLn $ "Error during wait:" ++ show e
    Right True -> when log (putStrLn "")
    Right False -> do
        when log (putStr ".")
        threadDelay (100*1000)
        waitUntilCompletion apiKey linId log


{-|
Select a Datacenter, a Plan, a Linux distribution and kernel from all Linode offering.
-}
select :: ApiKey -> LinodeCreationOptions -> ExceptT LinodeError IO (Datacenter, Distribution, Plan, Kernel)
select apiKey options = (,,,) <$>
  fetchAndSelect (runExceptT $ getDatacenters apiKey) (find ((== T.pack (datacenterChoice options)) . datacenterName)) "datacenter" <*>
  fetchAndSelect (runExceptT $ getDistributions apiKey) (distributionSelect options) "distribution" <*>
  fetchAndSelect (runExceptT $ getPlans apiKey) (find ((== T.pack (planChoice options)) . planName)) "plan" <*>
  fetchAndSelect (runExceptT $ getKernels apiKey) (kernelSelect options) "kernel"


{-|
Pick one public address of the Linode Instance
-}
publicAddress :: Linode -> Maybe Address
publicAddress = headMay . sortBy (comparing ip) . filter isPublic . linodeAddresses

{-|
Example of Linode creation. It expects the apiKey and id_rsa.pub files in the current directory.
-}
exampleCreateOneLinode :: IO (Maybe Linode)
exampleCreateOneLinode = do
  apiKey <- fmap (head . words) (readFile "apiKey")
  sshPublicKey <- readFile "id_rsa.pub"
  let options = defaultLinodeCreationOptions {
    datacenterChoice = "atlanta",
    planChoice = "Linode 1024",
    sshKey = Just sshPublicKey
  }
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
Example of Linodes creation. It expects the apiKey and id_rsa.pub files in the current directory.
-}
exampleCreateTwoLinodes :: IO (Maybe [Linode])
exampleCreateTwoLinodes = do
  sshPublicKey <- readFile "id_rsa.pub"
  apiKey <- fmap (head . words) (readFile "apiKey")
  let options = defaultLinodeCreationOptions {
    datacenterChoice = "atlanta",
    planChoice = "Linode 1024",
    sshKey = Just sshPublicKey
  }
  c <- createCluster apiKey options 2 True
  case c of
    Left errors -> do
      print ("error(s) in cluster creation" ++ show errors)
      return Nothing
    Right linodes -> do
      mapM_ (traverse_ (\a -> waitForSSH a >> setup a) . publicAddress) linodes
      return (Just linodes)
  where setup address = P.callCommand $ "scp TODO root@" <> ip address <> ":/root"
