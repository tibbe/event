{-# LANGUAGE ForeignFunctionInterface, MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

module System.Event.Thread
    (
      ensureIOManagerIsRunning
    , threadWaitRead
    , threadWaitWrite
    , threadDelay
    , registerDelay
    ) where

import GHC.Base
import GHC.IOBase
import Control.Monad (forM_, liftM2)
import Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar, newMVar,
                                putMVar, takeMVar)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.Types (CInt)
import GHC.Conc (TVar, ThreadId(..), ThreadStatus(..), atomically, forkOnIO,
                 labelThread, newTVar, numCapabilities, threadStatus, writeTVar)
import qualified GHC.Conc as Conc
import System.Event.Manager (Event, EventManager, evtRead, evtWrite, loop,
                             new, registerFd, unregisterFd_, registerTimeout)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (Fd)

-- | Suspends the current thread for a given number of microseconds
-- (GHC only).
--
-- There is no guarantee that the thread will be rescheduled promptly
-- when the delay has expired, but the thread will never continue to
-- run /earlier/ than specified.
threadDelay :: Int -> IO ()
threadDelay time
  | threaded  = waitForDelayEvent time
  | otherwise = Conc.threadDelay time

myEventManager :: IO (IORef (Managing EventManager))
myEventManager = do
  Manager e _i <- unsafeReadIOArray managers =<< myCapability
  return e

waitForDelayEvent :: Int -> IO ()
waitForDelayEvent usecs = do
  Running mgr <- readIORef =<< myEventManager
  m <- newEmptyMVar
  _ <- registerTimeout mgr (usecs `div` 1000) (putMVar m ())
  takeMVar m

-- | Set the value of returned TVar to True after a given number of
-- microseconds. The caveats associated with threadDelay also apply.
--
registerDelay :: Int -> IO (TVar Bool)
registerDelay usecs
  | threaded  = waitForDelayEventSTM usecs
  | otherwise = error "registerDelay: requires -threaded"

waitForDelayEventSTM :: Int -> IO (TVar Bool)
waitForDelayEventSTM usecs = do
  t <- atomically $ newTVar False
  Running mgr <- readIORef =<< myEventManager
  _ <- registerTimeout mgr (usecs `div` 1000) . atomically $ writeTVar t True
  return t   

-- | Block the current thread until data is available to read from the
-- given file descriptor.
threadWaitRead :: Fd -> IO ()
threadWaitRead fd
  | threaded  = threadWait evtRead fd
  | otherwise = Conc.threadWaitRead fd

-- | Block the current thread until the given file descriptor can
-- accept data to write.
threadWaitWrite :: Fd -> IO ()
threadWaitWrite fd
  | threaded  = threadWait evtWrite fd
  | otherwise = Conc.threadWaitWrite fd 

data Managing a = None
                | Running {-# UNPACK #-} !a

threadWait :: Event -> Fd -> IO ()
threadWait evt fd = do
  m <- newEmptyMVar
  Running mgr <- readIORef =<< myEventManager
  _ <- registerFd mgr (\reg _ -> unregisterFd_ mgr reg >> putMVar m ()) fd evt
  takeMVar m

data Manager = Manager {
      _evtMgr :: {-# UNPACK #-} !(IORef (Managing EventManager))
    , _ioMgr  :: {-# UNPACK #-} !(MVar (Managing ThreadId))
    }

managers :: IOArray Int Manager
managers = unsafePerformIO $ do
             a <- newIOArray (0,numCapabilities-1) undefined
             forM_ [0..numCapabilities-1] $ \i -> do
               m <- liftM2 Manager (newIORef None) (newMVar None)
               unsafeWriteIOArray a i m
             return a
{-# NOINLINE managers #-}

ensureIOManagerIsRunning :: IO ()
ensureIOManagerIsRunning = forM_ [0..numCapabilities-1] ensureManagerIsRunning

ensureManagerIsRunning :: Int -> IO ()
ensureManagerIsRunning n = do
  Manager eventManager ioManager <- unsafeReadIOArray managers n
  modifyMVar_ ioManager $ \old -> do
    maybeMgr <- readIORef eventManager
    mgr <- case maybeMgr of
             Running m -> return m
             None      -> do m <- new
                             writeIORef eventManager $! Running m
                             return m
    let create = do
          t <- forkOnIO n $ loop mgr
          labelThread t ("IOManager-" ++ show n)
          return $! Running t
    case old of
      None                -> create
      st@(Running t) -> do
        s <- threadStatus t
        case s of
          ThreadFinished -> create
          ThreadDied     -> create
          _other         -> return st

-- This should really report the number of the capability on which
-- we're currently executing, but it can't because that's not made
-- public by GHC's RTS.  In principle, this hack should work well
-- enough.
myCapability :: IO Int
myCapability
    | numCapabilities == 1 = return 0
    | otherwise = do
        ThreadId i <- myThreadId
        return $! (fromIntegral (getThreadId i) `mod` numCapabilities)
  where
    myThreadId = IO $ \s ->
      case (myThreadId# s) of (# s1, tid #) -> (# s1, ThreadId tid #)

foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool

foreign import ccall unsafe "rts_getThreadId" getThreadId :: ThreadId# -> CInt
