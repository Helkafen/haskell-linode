# haskell-linode

[![Build Status](https://travis-ci.org/Helkafen/haskell-linode.svg?branch=master)](https://travis-ci.org/Helkafen/haskell-linode)

Haskell bindings to the Linode API. Rent servers hourly or monthly.

This package contains some helpers to create and configure [Linode](https://www.linode.com) instances. The API key can be created on the Linode website.

Usage example:

```
import Network.Linode
import Data.List (find)
import qualified System.Process as P

main :: IO()
main = do
  apiKey <- fmap (head . words) (readFile "apiKey")
  sshPublicKey <- readFile "id_rsa.pub"
  let log = True
  let options = defaultLinodeCreationOptions {
    datacenterSelect = find ((=="atlanta") . datacenterName),
    planSelect = find ((=="Linode 2048") . planName),
    sshKey = Just sshPublicKey
  }
  l <- createLinode apiKey log options
  print l
  traverse_ (\a -> waitForSSH a >> setup a) (publicAddress l) -- Setup the instance when the ssh connexion is ready

setup address = P.callCommand $ "scp yourfile root@" <> ip address <> ":/root"
```
