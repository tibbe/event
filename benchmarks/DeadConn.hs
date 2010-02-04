{-# LANGUAGE CPP, ForeignFunctionInterface, OverloadedStrings #-}

-- A simple tool that creates a number of "dead" connections to a
-- server.  A dead connection is a connection that doesn't transmit
-- any data but stays connected.  This tool is useful to simulate a
-- number of slow/idle connections to a server.

import Args (ljust, nonNegative, parseArgs, positive, theLast)
import EventSocket (connect, sendAll)

import Control.Monad (replicateM_)
import Data.ByteString.Char8 ()
import Data.Function (on)
import Data.Monoid (Monoid(..), Last(..))
import Network.Socket (AddrInfo(..), Family(..), SocketType(..), defaultHints,
                       defaultProtocol, getAddrInfo, socket, withSocketsDo)
import System.Console.GetOpt (ArgDescr(ReqArg), OptDescr(..))
import System.Environment (getArgs)
import System.Event.Thread (ensureIOManagerIsRunning, threadDelay)
import System.Posix.Resource (ResourceLimit(..), ResourceLimits(..),
                              Resource(..), setResourceLimit)

main = withSocketsDo $ do
    (cfg, _) <- parseArgs defaultConfig defaultOptions =<< getArgs
    let numConns = theLast cfgNumConns cfg
        host     = theLast cfgHost cfg
        port     = theLast cfgPort cfg
        lim      = ResourceLimit $ fromIntegral numConns + 50
        myHints  = defaultHints { addrSocketType = Stream }

    ensureIOManagerIsRunning
    setResourceLimit ResourceOpenFiles
        ResourceLimits { softLimit = lim, hardLimit = lim }

    addrinfos <- getAddrInfo (Just myHints) (Just host) (Just $ show port)
    let addr = head addrinfos

    putStrLn $ "Establishing " ++ show numConns ++ " connections to " ++
        host ++ ":" ++ show port ++ "..."
    replicateM_ numConns $ do
        sock <- socket (addrFamily addr) (addrSocketType addr)
                (addrProtocol addr)
        connect sock (addrAddress addr)
        sendAll sock request
    putStrLn $ show numConns ++ " connections established"

    -- Block process forever.
    threadDelay maxBound

request = "GET / HTTP/1.1\r\nHost: www.test.com\r\n\r\n"

------------------------------------------------------------------------
-- Configuration

data Config = Config {
      cfgNumConns :: Last Int
    , cfgHost     :: Last String
    , cfgPort     :: Last Int
    }

defaultConfig :: Config
defaultConfig = Config
    { cfgNumConns = ljust 50
    , cfgHost     = ljust "localhost"
    , cfgPort     = ljust 3000
    }

instance Monoid Config where
    mempty = Config
        { cfgNumConns = mempty
        , cfgHost     = mempty
        , cfgPort     = mempty
        }

    mappend a b = Config
        { cfgNumConns = app cfgNumConns a b
        , cfgHost     = app cfgHost a b
        , cfgPort     = app cfgPort a b
        }
      where
        app :: (Monoid b) => (a -> b) -> a -> a -> b
        app = on mappend

defaultOptions :: [OptDescr (IO Config)]
defaultOptions = [
      Option ['n'] ["connections"]
          (ReqArg (nonNegative "number of connections" $ \n ->
               mempty { cfgNumConns = n }) "N")
          "number of connections"
    , Option ['h'] ["host"]
          (ReqArg (\s -> return $ mempty { cfgHost = ljust s }) "HOST")
          "server address"
    , Option ['p'] ["port"]
          (ReqArg (positive "server port" $ \n ->
               mempty { cfgPort = n }) "N")
          "server port"
    ]
