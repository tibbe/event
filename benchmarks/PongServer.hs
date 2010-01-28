{-# LANGUAGE CPP, OverloadedStrings #-}

-- Requires the network-bytestring library.
--
-- Start server and run
--   httperf --server=localhost --port=5002 --uri=/ --num-conns=10000

import Args (ljust, parseArgs, positive, theLast)

import Control.Concurrent (forkIO)
import Data.ByteString.Char8 ()
import Data.Function (on)
import Data.Monoid (Monoid(..), Last(..))
import Network (PortID(..), listenOn)
import Network.Socket (Socket, sClose)
#if 1
import EventSocket (accept, recv, sendAll)
#else
import Network.Socket (accept)
import Network.Socket.ByteString (recv, sendAll)
#endif
import System.Console.GetOpt (ArgDescr(ReqArg), OptDescr(..))
import System.Environment (getArgs)
import System.Event.Thread (ensureIOManagerIsRunning)

main = do
    (cfg, _) <- parseArgs defaultConfig defaultOptions =<< getArgs
    let port = fromIntegral $ theLast cfgPort cfg

    ensureIOManagerIsRunning
    sock <- listenOn $ PortNumber port
    acceptConnections sock

acceptConnections :: Socket -> IO ()
acceptConnections sock = loop
  where
    loop = do
        (c,_) <- accept sock
        forkIO $ client c
        loop

client :: Socket -> IO ()
client sock = do
    recv sock 4096
    sendAll sock msg
    sClose sock

msg = "HTTP/1.0 200 OK\r\n\r\nContent-Length: 5\r\n\r\nPong!\r\n"

------------------------------------------------------------------------
-- Configuration

data Config = Config {
      cfgPort :: Last Int
    }

defaultConfig :: Config
defaultConfig = Config
    { cfgPort = ljust 5002
    }

instance Monoid Config where
    mempty = Config
        { cfgPort     = mempty
        }

    mappend a b = Config
        { cfgPort     = app cfgPort a b
        }
      where
        app :: (Monoid b) => (a -> b) -> a -> a -> b
        app = on mappend

defaultOptions :: [OptDescr (IO Config)]
defaultOptions = [
      Option ['p'] ["port"]
          (ReqArg (positive "server port" $ \n ->
               mempty { cfgPort = n }) "N")
          "server port"
    ]
