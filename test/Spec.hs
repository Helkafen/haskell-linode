{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as B
import qualified Data.Map             as M
import qualified Data.Text.Encoding   as E
import           Network.Linode.Types
import           Network.Linode.Parsing
import           Test.Tasty
import           Test.Tasty.HUnit
--import qualified Test.Tasty.QuickCheck   as QC


testAuthenticationError :: Assertion
testAuthenticationError =  assertEqual "Error while parsing an Authentication failure" (Left AuthenticationFailed) x
  where x = parseResponse "{\"ERRORARRAY\":[{\"ERRORCODE\":4,\"ERRORMESSAGE\":\"Authentication failed\"}],\"DATA\":{},\"ACTION\":\"avail.datacenters\"}" :: Either LinodeError [Datacenter]

testDatacenterList :: Assertion
testDatacenterList =  assertEqual "Error while parsing an datacenter list" (Right [Datacenter (DatacenterId 2) "Dallas, TX, USA" "dallas"]) x
  where x = parseResponse "{\"ERRORARRAY\":[],\"DATA\":[{\"LOCATION\":\"Dallas, TX, USA\",\"DATACENTERID\":2,\"ABBR\":\"dallas\"}],\"ACTION\":\"avail.datacenters\"}"

testDistributionList :: Assertion
testDistributionList = assertEqual "Error while parsing a distribution list" (Right [Distribution (DistributionId 142) "Arch Linux 2015.08" True 800 False]) x
  where x = parseResponse "{\"ERRORARRAY\":[],\"DATA\":[{\"REQUIRESPVOPSKERNEL\":0,\"DISTRIBUTIONID\":142,\"IS64BIT\":1,\"LABEL\":\"Arch Linux 2015.08\",\"MINIMAGESIZE\":800,\"CREATE_DT\":\"2015-08-24 11:17:18.0\"}],\"ACTION\":\"avail.distributions\"}" :: Either LinodeError [Distribution]

testKernelList :: Assertion
testKernelList = assertEqual "Error while parsing a kernel list" (Right [Kernel (KernelId 137) "Latest 32 bit (4.1.5-x86-linode80)" True True True]) x
  where x = parseResponse "{\"ERRORARRAY\":[],\"DATA\":[{\"ISKVM\":1,\"LABEL\":\"Latest 32 bit (4.1.5-x86-linode80)\",\"ISXEN\":1,\"ISPVOPS\":1,\"KERNELID\":137}],\"ACTION\":\"avail.kernels\"}" :: Either LinodeError [Kernel]

testPlanList :: Assertion
testPlanList = assertEqual "Error while parsing a plan list" (Right [Plan (PlanId 1) "Linode 1024" 1024 24 2000 0.0150 (M.fromList [(DatacenterId 3,500),(DatacenterId 2,400)])]) x
  where x = parseResponse "{\"ERRORARRAY\":[],\"DATA\":[{\"CORES\":1,\"PRICE\":10.00,\"RAM\":1024,\"XFER\":2000,\"PLANID\":1,\"LABEL\":\"Linode 1024\",\"AVAIL\":{\"3\":500,\"2\":400},\"DISK\":24,\"HOURLY\":0.0150}],\"ACTION\":\"avail.linodeplans\"}" :: Either LinodeError [Plan]

testAccountInfo :: Assertion
testAccountInfo = assertEqual "Error while parsing some account info" (Right $ AccountInfo 2000 1 0 False 0 "prepay") x
  where x = parseResponse "{\"ERRORARRAY\":[],\"DATA\":{\"TRANSFER_USED\":1,\"BALANCE\":0.0000,\"TRANSFER_BILLABLE\":0,\"BILLING_METHOD\":\"prepay\",\"TRANSFER_POOL\":2000,\"ACTIVE_SINCE\":\"2011-03-10 19:18:43.0\",\"MANAGED\":false},\"ACTION\":\"account.info\"}" :: Either LinodeError AccountInfo

testInstanceList :: Assertion
testInstanceList = assertEqual "Error while parsing some account info" (Right [Instance {instanceId = LinodeId 8098, instanceName = "api-node3", instanceDatacenterId = DatacenterId 5, instancePlanId = PlanId 1, instanceRAM = 1024, instanceHD = 40960, instanceTransfer = 2000, instanceBackupEnabled = True, instanceStatus = PoweredOff}]) x
  where x = parseResponse "{\"ERRORARRAY\":[],\"DATA\":[{\"LINODEID\":8098, \"LABEL\": \"api-node3\", \"DATACENTERID\": 5, \"PLANID\":1, \"TOTALRAM\":1024, \"TOTALHD\":40960, \"TOTALXFER\":2000, \"BACKUPSENABLED\":1, \"STATUS\":2}],\"ACTION\":\"linode.list\"}" :: Either LinodeError [Instance]

testInstanceCreation :: Assertion
testInstanceCreation = assertEqual "Error while parsing the instanceId after a call to linode.create" (Right $ CreatedLinode (LinodeId 1449138)) x
  where x = parseResponse "{\"ERRORARRAY\":[],\"DATA\":{\"LinodeID\":1449138},\"ACTION\":\"linode.create\"}"

testDiskCreation :: Assertion
testDiskCreation = assertEqual "Error while parsing the diskId and jobId after creating a disk" (Right $ CreatedDisk (DiskId 55647) (JobId 1298)) x
  where x = parseResponse "{\"ERRORARRAY\":[],\"ACTION\":\"linode.disk.createFromDistribution\",\"DATA\":{\"JobID\":1298,\"DiskID\":55647}}"

testJobWait :: Assertion
testJobWait = assertEqual "Error while parsing a waiting job list" (Right [WaitingJob (JobId 29046473) (LinodeId 1450724) True, WaitingJob (JobId 29046472) (LinodeId 1450724) False]) x
  where x = parseResponse "{\"ERRORARRAY\":[],\"DATA\":[{\"HOST_START_DT\":\"\",\"HOST_MESSAGE\":\"\",\"ENTERED_DT\":\"2015-11-08 10:38:15.0\",\"HOST_FINISH_DT\":\"\",\"LABEL\":\"Disk Create From Distribution - Debian 8.1\",\"JOBID\":29046473,\"HOST_SUCCESS\":1,\"ACTION\":\"fs.create.from.distro\",\"LINODEID\":1450724,\"DURATION\":\"\"},{\"HOST_START_DT\":\"\",\"HOST_MESSAGE\":\"\",\"ENTERED_DT\":\"2015-11-08 10:38:14.0\",\"HOST_FINISH_DT\":\"\",\"LABEL\":\"Linode Initial Configuration\",\"JOBID\":29046472,\"HOST_SUCCESS\":\"\",\"ACTION\":\"linode.create\",\"LINODEID\":1450724,\"DURATION\":\"\"}],\"ACTION\":\"linode.job.list\"}"

testParsingWithNoData :: Assertion
testParsingWithNoData = assertEqual "Parsing no data should fail" x (Left expectedError)
  where x = parseResponse body :: Either LinodeError Datacenter
        body = "{\"ERRORARRAY\":[], \"ACTION\":\"avail.datacenters\"}"
        expectedError = DeserializationError (E.decodeUtf8 $ B.toStrict body)

testParsingEmptyResponse :: Assertion
testParsingEmptyResponse = assertEqual "Parsing an empty response should fail" x (Left expectedError)
  where x = parseResponse "" :: Either LinodeError Datacenter
        expectedError = DeserializationError ""

tests :: TestTree
tests = testGroup "Parsing tests" [
  testCase "Error while parsing an Authentication failure" testAuthenticationError,
  testCase "Error while parsing a datacenter list" testDatacenterList,
  testCase "Error while parsing a distribution list" testDistributionList,
  testCase "Error while parsing a kernel list" testKernelList,
  testCase "Error while parsing a plan list" testPlanList,
  testCase "Error while parsing some account info" testAccountInfo,
  testCase "Error while parsing an instance list" testInstanceList,
  testCase "Error while parsing the instanceId after a call to linode.create" testInstanceCreation,
  testCase "Error while parsing the diskId and jobId after creating a disk" testDiskCreation,
  testCase "Error while parsing a waiting job list" testJobWait,
  testCase "Parsing no data should fail" testParsingWithNoData,
  testCase "Parsing an empty response should fail" testParsingEmptyResponse
  ]


main :: IO ()
main = defaultMain tests
