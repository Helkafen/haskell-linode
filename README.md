# haskell-linode

[![Build Status](https://travis-ci.org/Helkafen/haskell-linode.svg?branch=master)](https://travis-ci.org/Helkafen/haskell-linode)

Haskell bindings to the Linode API. Rent servers hourly or monthly.

This package contains some helpers to create and configure [Linode](https://www.linode.com) instances. The API key can be created on the Linode website.

Usage example. We want to create one Linode instance in Atlanta with 1GB of RAM:

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
    datacenterChoice = "atlanta",
    planChoice = "Linode 1024",
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
