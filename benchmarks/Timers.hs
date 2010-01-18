-- Benchmark that registers N timeouts and waits for them to expire.

import Args (ljust, parseArgs, positive, theLast)
import Control.Concurrent (MVar, forkIO, takeMVar, newEmptyMVar, putMVar)
import Control.Monad (replicateM_, when)
import Data.Function (on)
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.Monoid (Monoid(..), Last(..))
import System.Event (TimeoutKey, loop, new, registerTimeout)
import System.Console.GetOpt (ArgDescr(ReqArg), OptDescr(..))
import System.Environment (getArgs)

data Config = Config {
      cfgNumTimeouts :: Last Int
    }

defaultConfig :: Config
defaultConfig = Config
    { cfgNumTimeouts    = ljust 1000
    }

instance Monoid Config where
    mempty = Config
        { cfgNumTimeouts    = mempty
        }

    mappend a b = Config
        { cfgNumTimeouts = app cfgNumTimeouts a b
        }
      where app = on mappend

defaultOptions :: [OptDescr (IO Config)]
defaultOptions = [
      Option ['n'] ["timeouts"]
          (ReqArg (positive "number of timeouts" $ \n ->
               mempty { cfgNumTimeouts = n }) "N")
          "number of timeouts to use"
    ]

callback :: MVar () -> IORef Int -> Config -> IO ()
callback done nref cfg = do
    a <- atomicModifyIORef nref (\a -> let !b = a+1 in (b,b))
    when (a >= numTimeouts) $ putMVar done ()
  where
    numTimeouts = theLast cfgNumTimeouts cfg

main :: IO ()
main = do
    (cfg, _) <- parseArgs defaultConfig defaultOptions =<< getArgs
    let numTimeouts = theLast cfgNumTimeouts cfg

    mgr <- new
    forkIO $ loop mgr
    nref <- newIORef 0
    done <- newEmptyMVar
    let oneMs = 1
    replicateM_ numTimeouts $ registerTimeout mgr oneMs (callback done nref cfg)
    takeMVar done
