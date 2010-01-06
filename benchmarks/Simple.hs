-- Flow:
--
-- 1. Create N pipes.
--
-- Modelled after:
-- http://levent.svn.sourceforge.net/viewvc/levent/trunk/libevent/test/bench.c

module Main where

import Args (ljust, parseArgs, positive, theLast)
import Control.Concurrent (MVar, forkIO, takeMVar, newEmptyMVar, putMVar)
import Control.Monad (forM_, replicateM, when)
import Data.Array.Unboxed (UArray, listArray)
import Data.Function (on)
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.Int (Int32)
import Data.Monoid (Monoid(..), Last(..), getLast)
import Foreign.C.Error (throwErrnoIfMinus1Retry)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CChar)
import System.Console.GetOpt (ArgDescr(ReqArg), OptDescr(..))
import System.Environment (getArgs)
import System.Event (Event(..), loop, new, registerFd)
import System.Posix.IO (createPipe)
import System.Posix.Resource (ResourceLimit(..), ResourceLimits(..),
                              Resource(..), setResourceLimit)
import System.Posix.Internals (c_close, c_read, c_write)
import System.Posix.Types (Fd(..))

data Config = Config {
      cfgNumPipes :: Last Int
    }

defaultConfig :: Config
defaultConfig = Config {
                  cfgNumPipes = ljust 1024
                }

instance Monoid Config where
    mempty  = Config {
                cfgNumPipes = mempty
              }
    mappend a b = Config {
                    cfgNumPipes = app cfgNumPipes a b
                  }
        where app = on mappend

defaultOptions :: [OptDescr (IO Config)]
defaultOptions = [
  Option ['n'] ["num-pipes"]
         (ReqArg (positive "number of pipes" $ \n -> mempty { cfgNumPipes = n }) "N")
          "number of pipes to use"
 ]

readCallback :: MVar () -> IORef Int -> Fd -> [Event] -> IO ()
readCallback done ref fd _ = do
  a <- atomicModifyIORef ref (\a -> let !b = a+1 in (b,b))
  print ("read",fd,a)
  if a > 10
    then do
      close fd
      putMVar done ()
    else do
      readByte fd

writeCallback :: IORef Int -> Fd -> [Event] -> IO ()
writeCallback ref fd _ = do
  a <- atomicModifyIORef ref (\a -> let !b = a+1 in (b,b))
  print ("write",fd,a)
  if a > 10
    then close fd
    else do
      writeByte fd

main :: IO ()
main = do
    (cfg, args) <- parseArgs defaultConfig defaultOptions =<< getArgs
    let numPipes = theLast cfgNumPipes cfg
        lim = ResourceLimit $ fromIntegral numPipes * 2 + 50
    setResourceLimit ResourceOpenFiles
        ResourceLimits { softLimit = lim, hardLimit = lim }

    pipePairs <- replicateM numPipes createPipe
    print pipePairs
    let pipes = concatMap (\(r,w) -> [r,w]) pipePairs

    mgr <- new
    forkIO $ loop mgr
    rref <- newIORef 0
    wref <- newIORef 0
    done <- newEmptyMVar
    forM_ pipePairs $ \(r,w) -> do
      registerFd mgr (readCallback done rref r) r [Read]
      registerFd mgr (writeCallback wref w) w [Write]

    let pipeArray :: UArray Int Int32
        pipeArray = listArray (0, numPipes) . map fromIntegral $ pipes
    takeMVar done

readByte :: Fd -> IO ()
readByte (Fd fd) =
    alloca $ \p -> do
      n <- throwErrnoIfMinus1Retry "readByte" $ c_read fd p 1
      when (n /= 1) . error $ "readByte returned " ++ show n

writeByte :: Fd -> IO ()
writeByte (Fd fd) =
    alloca $ \p -> do
      n <- throwErrnoIfMinus1Retry "writeByte" $ c_write fd p 1
      when (n /= 1) . error $ "writeByte returned " ++ show n
      
close :: Fd -> IO ()
close (Fd fd) = do
  c_close fd
  return ()
