module Tools.Temporary where

import Prelude

import System.IO.Unsafe

clientId, clientSecret :: String
[clientId, clientSecret] = map (head . lines . unsafePerformIO . readFile)
	["clientId.txt", "clientSecret.txt"]
