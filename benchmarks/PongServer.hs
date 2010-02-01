{-# LANGUAGE CPP, OverloadedStrings #-}

-- Requires the network-bytestring library.
--
-- Start server and run
--   httperf --server=localhost --port=5002 --uri=/ --num-conns=10000
-- or
--   ab -n 10000 -c 100 http://localhost:5002/
--
-- The -d option (drain a socket) seems to be incompatible with
-- apachebench, causing it to hang.

import Args (ljust, parseArgs, positive, theLast)
import Control.Concurrent (forkIO)
import Control.Monad (unless, when)
import Data.ByteString.Char8 ()
import Data.Function (on)
import Data.Monoid (Monoid(..), Last(..))
import Network.Socket hiding (accept, recv)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
#if 0
import EventSocket (accept, recv, sendAll)
#else
import Network.Socket (accept)
import Network.Socket.ByteString (recv, sendAll)
#endif
import System.Console.GetOpt (ArgDescr(NoArg, ReqArg), OptDescr(..))
import System.Environment (getArgs)
import System.Event.Thread (ensureIOManagerIsRunning)
import System.Posix.Resource (ResourceLimit(..), ResourceLimits(..),
                              Resource(..), setResourceLimit)

main = do
  (cfg, _) <- parseArgs defaultConfig defaultOptions =<< getArgs
  let port = theLast cfgPort cfg
      lim  = ResourceLimit . fromIntegral . theLast cfgMaxConns $ cfg
      myHints = defaultHints { addrFlags = [AI_PASSIVE]
                             , addrSocketType = Stream }
  ensureIOManagerIsRunning
  setResourceLimit ResourceOpenFiles
      ResourceLimits { softLimit = lim, hardLimit = lim }
  (ai:_) <- getAddrInfo (Just myHints) Nothing (Just port)
  sock <- socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai)
  setSocketOption sock ReuseAddr 1
  bindSocket sock (addrAddress ai)
  listen sock maxListenQueue
  acceptConnections cfg sock

acceptConnections :: Config -> Socket -> IO ()
acceptConnections cfg sock = loop
  where
    loop = do
        (c,_) <- accept sock
        forkIO $ client (theLast cfgDrain cfg) c
        loop

client :: Bool -> Socket -> IO ()
client drain sock = do
  s <- recv sock 4096
  sendAll sock msg
  when (drain && not (S.null s)) drainSocket
  sClose sock
 where
  msg = "HTTP/1.0 200 OK\r\nConnection: Close\r\nContent-Length: 5\r\n\r\nPong!"
  drainSocket = do
    s <- recv sock 4096
    unless (S.null s) drainSocket

------------------------------------------------------------------------
-- Configuration

data Config = Config {
      cfgDrain    :: Last Bool
    , cfgPort     :: Last String
    , cfgMaxConns :: Last Int
    }

defaultConfig :: Config
defaultConfig = Config { cfgDrain    = ljust False
                       , cfgPort     = ljust "5002"
                       , cfgMaxConns = ljust 256
                       }

instance Monoid Config where
    mempty = Config { cfgDrain    = mempty
                    , cfgPort     = mempty
                    , cfgMaxConns = mempty
                    }

    mappend a b = Config { cfgDrain    = app cfgDrain a b
                         , cfgPort     = app cfgPort a b
                         , cfgMaxConns = app cfgMaxConns a b
                         }
      where app :: (Monoid b) => (a -> b) -> a -> a -> b
            app = on mappend

defaultOptions :: [OptDescr (IO Config)]
defaultOptions = [
      Option ['d'] ["drain"]
          (NoArg (return mempty { cfgDrain = ljust True }))
          "drain request entirely"
    , Option ['p'] ["port"]
          (ReqArg (\s -> return mempty { cfgPort = ljust s }) "N")
          "server port"
    , Option ['m'] ["max-connections"]
          (ReqArg (positive "maximum number of connections" $ \n ->
               mempty { cfgMaxConns = n }) "N")
          "maximum number of concurrent connections"
    ]
