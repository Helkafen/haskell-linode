# haskell-linode

[![Build Status](https://travis-ci.org/Helkafen/haskell-linode.svg?branch=master)](https://travis-ci.org/Helkafen/haskell-linode)

Haskell bindings to the Linode API. Rent servers hourly or monthly.

This package contains some helpers to create and configure [Linode](https://www.linode.com) instances. The API key can be created on the Linode website.

Usage example:

```
{-# LANGUAGE OverloadedStrings #-}

import Network.Linode
import Data.List (find)
import qualified System.Process as P
import Data.Foldable (traverse_)
import Data.Monoid ((<>))

main :: IO()
main = do
  apiKey <- fmap (head . words) (readFile "apiKey")
  sshPublicKey <- readFile "id_rsa.pub"
  let options = defaultLinodeCreationOptions {
    datacenterSelect = find ((=="atlanta") . datacenterName),
    planSelect = find ((=="Linode 1024") . planName),
    sshKey = Just sshPublicKey
  }
  c <- createLinode apiKey True options
  case c of
    Left err -> print err
    Right linode -> do
      traverse_ (\a -> waitForSSH a >> setup a) (publicAddress linode)
      print linode

setup address = P.callCommand $ "scp TODO root@" <> ip address <> ":/root"
```
