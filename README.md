# haskell-linode
Haskell bindings to the Linode API.

This package contains some helpers to create and configure Linode instances. The API key can be created on the Linode website.

Usage example:

```
import Network.Linode
import Data.List (find)

main :: IO()
main = do
  apiKey <- fmap (filter (/= '\n')) (readFile "apiKey")
  let log = True
  let options = defaultLinodeCreationOptions {
    datacenterSelect = find ((=="atlanta") . datacenterName),
    planSelect = find ((=="Linode 2048") . planName),
  }
  c <- createLinode apiKey log options
  print c
```
