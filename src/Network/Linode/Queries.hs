module Network.Linode.Queries where

import           Data.List            (intercalate)
import           Data.Monoid          ((<>))

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

datacenterListQuery :: String -> String
datacenterListQuery = query "avail.datacenters"

distributionListQuery :: String -> String
distributionListQuery = query "avail.distributions"

kernelListQuery :: String -> String
kernelListQuery = query "avail.kernels"

planListQuery :: String -> String
planListQuery = query "avail.linodeplans"

accountInfoQuery :: String -> String
accountInfoQuery = query "account.info"

linodeListQuery :: String -> String
linodeListQuery = query "linode.list"

query :: String -> String -> String
query action apiKey = "https://api.linode.com/?api_key=" <> apiKey <> "&api_action=" <> action

createInstanceQuery :: String -> DatacenterId -> PlanId -> PaymentTerm -> String
createInstanceQuery apiKey (DatacenterId did) (PlanId pid) paymentTerm =
  query "linode.create" apiKey
    <> "&DatacenterID=" <> show did
    <> "&PlanID=" <> show pid
    <> "&PaymentTerm=" <> show (paymentTermToInt paymentTerm)

deleteInstanceQuery :: String -> InstanceId -> String
deleteInstanceQuery apiKey (InstanceId i) =
  query "linode.delete" apiKey <> "&LinodeId=" <> show i

createDistFromDistributionQuery :: String -> InstanceId -> DistributionId -> String -> Int -> String -> Maybe String -> String
createDistFromDistributionQuery apiKey (InstanceId i) (DistributionId d) label size pass sshKey =
  query "linode.disk.createfromdistribution" apiKey
    <> "&LinodeID=" <> show i
    <> "&DistributionID=" <> show d
    <> "&Label=" <> label
    <> "&Size=" <> show size
    <> "&rootPass=" <> pass
    <> case sshKey of
      Nothing -> ""
      Just k -> "&rootSSHKey=" <> k

createDiskQuery :: String -> InstanceId -> String -> DiskType -> Int -> String
createDiskQuery apiKey (InstanceId i) label diskType size =
  query "linode.disk.create" apiKey
    <> "&LinodeID=" <> show i
    <> "&Label=" <> label
    <> "&Type=" <> diskTypeToString diskType
    <> "&size=" <> show size

ipListQuery :: String -> InstanceId -> String
ipListQuery apiKey (InstanceId i) =
  query "linode.ip.list" apiKey
    <> "&LinodeID=" <> show i

jobListQuery :: String -> InstanceId -> String
jobListQuery apiKey (InstanceId i) =
  query "linode.job.list" apiKey
    <> "&LinodeID=" <> show i
    <> "&pendingOnly=true"

createConfigQuery :: String -> InstanceId -> KernelId -> String -> [DiskId] -> String
createConfigQuery apiKey (InstanceId i) (KernelId k) label disks =
  query "linode.config.create" apiKey
    <> "&LinodeID=" <> show i
    <> "&KernelID=" <> show k
    <> "&Label=" <> label
    <> "&DiskList=" <> disksList
    where disksList = intercalate "," $ take 9 $ map (show . unDisk) disks ++ repeat ""
          unDisk (DiskId i) = i


bootQuery :: String -> InstanceId -> ConfigId -> String
bootQuery apiKey (InstanceId i) (ConfigId c)=
  query "linode.boot" apiKey
    <> "&LinodeID=" <> show i
    <> "&ConfigID=" <> show c
