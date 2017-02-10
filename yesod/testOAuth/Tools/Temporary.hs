module Tools.Temporary where

import Prelude

import System.IO.Unsafe

gClientId, gClientSecret :: String
[gClientId, gClientSecret] = map (head . lines . unsafePerformIO . readFile)
	["g_clientId.txt", "g_clientSecret.txt"]
