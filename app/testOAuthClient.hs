{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans
import Data.Pipe
import System.Environment
import Network
import Network.TigHTTP.Client
import Network.TigHTTP.Types

import qualified Data.ByteString as BS

main :: IO ()
main = do
	addr : pth : _ <- getArgs
	h <- connectTo addr $ PortNumber 3000
	let RequestGet (Path p) v g = get addr 3000 pth
	r <- request h $ RequestGet (Path p) v g
	_ <- runPipe $ responseBody r =$= finally printP (putStrLn "")
	return ()

printP :: MonadIO m => Pipe BS.ByteString () m ()
printP = await >>= maybe (return ()) (\s -> liftIO (BS.putStr s) >> printP)
