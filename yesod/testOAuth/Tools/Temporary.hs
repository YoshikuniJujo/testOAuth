module Tools.Temporary (
	gClientId, gClientSecret,
	fClientId, fClientSecret,
	yClientId, yClientSecret
	) where

import Prelude

import System.IO.Unsafe

gClientId, gClientSecret :: String
[gClientId, gClientSecret] = map (head . lines . unsafePerformIO . readFile)
	["g_clientId.txt", "g_clientSecret.txt"]

fClientId, fClientSecret :: String
[fClientId, fClientSecret] = map (head . lines . unsafePerformIO . readFile)
	["f_clientId.txt", "f_clientSecret.txt"]

yClientId, yClientSecret, yClientIdSecret :: String
[yClientId, yClientSecret, yClientIdSecret] = map (concat . lines . unsafePerformIO . readFile)
	["y_clientId.txt", "y_clientSecret.txt", "y_clientIdSecret.txt"]
