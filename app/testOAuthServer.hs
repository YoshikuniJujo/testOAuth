module Main where

import Control.Monad
import Control.Concurrent
import Data.Pipe
import Data.Pipe.Lazy
import System.IO
import System.Environment
import Network
import Network.TigHTTP.Server
import Network.TigHTTP.Types

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
	as <- getArgs
	soc <- listenOn $ PortNumber 3000
	forever $ do
		(h, _, _) <- accept soc
		void . forkIO $ do
			req <- getRequest h
			print $ requestPath req
			let rb = requestBody req
			toLazy rb >>= print
			putResponse h
				. (response :: LBS.ByteString -> Response Pipe Handle)
				. LBS.fromChunks $ map BSC.pack as
