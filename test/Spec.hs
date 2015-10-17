{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import qualified Data.Map              as M
import           Network.Linode
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

testAuthenticationError :: Assertion
testAuthenticationError =  assertEqual "Parsing an Authentication failure" (Left AuthenticationFailed) x
  where x = parseListResponse "{\"ERRORARRAY\":[{\"ERRORCODE\":4,\"ERRORMESSAGE\":\"Authentication failed\"}],\"DATA\":{},\"ACTION\":\"avail.datacenters\"}" :: Either LinodeError [Datacenter]

testDatacenterList :: Assertion
testDatacenterList =  assertEqual "Parsing an datacenter list" (Right [Datacenter (DatacenterId 2) "Dallas, TX, USA" "dallas"]) x
  where x = parseListResponse "{\"ERRORARRAY\":[],\"DATA\":[{\"LOCATION\":\"Dallas, TX, USA\",\"DATACENTERID\":2,\"ABBR\":\"dallas\"}],\"ACTION\":\"avail.datacenters\"}"

testDistributionList :: Assertion
testDistributionList = assertEqual "Parsing a distribution list" (Right [Distribution (DistributionId 142) "Arch Linux 2015.08" True 800 False]) x
  where x = parseListResponse "{\"ERRORARRAY\":[],\"DATA\":[{\"REQUIRESPVOPSKERNEL\":0,\"DISTRIBUTIONID\":142,\"IS64BIT\":1,\"LABEL\":\"Arch Linux 2015.08\",\"MINIMAGESIZE\":800,\"CREATE_DT\":\"2015-08-24 11:17:18.0\"}],\"ACTION\":\"avail.distributions\"}" :: Either LinodeError [Distribution]

testKernelList :: Assertion
testKernelList = assertEqual "Parsing a kernel list" (Right [Kernel (KernelId 137) "Latest 32 bit (4.1.5-x86-linode80)" True True True]) x
  where x = parseListResponse "{\"ERRORARRAY\":[],\"DATA\":[{\"ISKVM\":1,\"LABEL\":\"Latest 32 bit (4.1.5-x86-linode80)\",\"ISXEN\":1,\"ISPVOPS\":1,\"KERNELID\":137}],\"ACTION\":\"avail.kernels\"}" :: Either LinodeError [Kernel]

testPlanList :: Assertion
testPlanList = assertEqual "Parsing a plan list" (Right [Plan (PlanId 1) "Linode 1024" 1024 24 2000 0.0150 (M.fromList [(DatacenterId 3,500),(DatacenterId 2,400)])]) x
  where x = parseListResponse "{\"ERRORARRAY\":[],\"DATA\":[{\"CORES\":1,\"PRICE\":10.00,\"RAM\":1024,\"XFER\":2000,\"PLANID\":1,\"LABEL\":\"Linode 1024\",\"AVAIL\":{\"3\":500,\"2\":400},\"DISK\":24,\"HOURLY\":0.0150}],\"ACTION\":\"avail.linodeplans\"}" :: Either LinodeError [Plan]

testAccountInfo :: Assertion
testAccountInfo = assertEqual "Parsing some account info" (Right $ AccountInfo 2000 1 0 False 0 "prepay") x
  where x = parseAccountInfo "{\"ERRORARRAY\":[],\"DATA\":{\"TRANSFER_USED\":1,\"BALANCE\":0.0000,\"TRANSFER_BILLABLE\":0,\"BILLING_METHOD\":\"prepay\",\"TRANSFER_POOL\":2000,\"ACTIVE_SINCE\":\"2011-03-10 19:18:43.0\",\"MANAGED\":false},\"ACTION\":\"account.info\"}" :: Either LinodeError AccountInfo

tests :: TestTree
tests = testGroup "Parsing tests" [
  testCase "Parsing an Authentication failure" testAuthenticationError,
  testCase "Parsing a datacenter list" testDatacenterList,
  testCase "Parsing a distribution list" testDistributionList,
  testCase "Parsing a kernel list" testKernelList,
  testCase "Parsing a plan list" testPlanList,
  testCase "Parsing some account info" testAccountInfo
  ]


main :: IO ()
main = defaultMain tests
