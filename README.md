# haskell-linode
Haskell bindings to the Linode API. Rent servers hourly or monthly.

This package contains some helpers to create and configure Linode instances. The API key can be created on the Linode website.

Usage example:

```
import Network.Linode
import Data.List (find)
import qualified System.Process as P

main :: IO()
main = do
  apiKey <- fmap (head . words) (readFile "apiKey")
  idrsa <- readFile "idrsa"
  let log = True
  let options = defaultLinodeCreationOptions {
    datacenterSelect = find ((=="atlanta") . datacenterName),
    planSelect = find ((=="Linode 2048") . planName),
    sshKey = Just idrsa
  }
  l <- createLinode apiKey log options
  print l
  traverse_ (\a -> waitForSSH a >> setup a) (publicAddress l) -- Setup the instance when the ssh connexion is ready

setup address = P.callCommand $ "scp yourfile root@" <> ip address <> ":/root"
```
