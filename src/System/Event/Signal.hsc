{-# LANGUAGE BangPatterns, CPP, EmptyDataDecls, ForeignFunctionInterface,
    RecordWildCards #-}

module System.Event.Signal
    (
      SignalManager
    , Signal
    , SignalHandler
    , SignalKey(skSignal)
    , blockAllSignals
    , new
    , loop
    , registerHandler_
    , registerHandler
    , unregisterHandler_
    , unregisterHandler
    , wakeManager
    ) where

#include "EventConfig.h"

#include <signal.h>
#include <pthread.h>

import Control.Concurrent (runInBoundThread)
import Control.Monad (forM_, liftM2, liftM5, when)
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import Foreign.C.Error (throwErrnoIfMinus1, throwErrnoIfMinus1_)
import Foreign.C.Types (CInt, CLong)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))
import Prelude
import System.Event.Unique (Unique, UniqueSource, newUnique, newSource)
import qualified System.Event.IntMap as IM

data SignalManager = SignalManager {
      smHandlers     :: {-# UNPACK #-} !(IORef (IM.IntMap [Handler]))
    , smUniqueSource :: {-# UNPACK #-} !UniqueSource
    , smKeepRunning  :: {-# UNPACK #-} !(IORef Bool)
    , smWakeup       :: {-# UNPACK #-} !(IORef Bool)
    , smThread       :: {-# UNPACK #-} !(ForeignPtr PosixThread)
    }

type SignalHandler = SignalKey -> IO ()

type Signal = CInt

data SignalKey = SK {
      skSignal :: {-# UNPACK #-} !Signal
    , skUnique :: {-# UNPACK #-} !Unique
    } deriving (Eq)

data Handler = Handler {
      haKey     :: {-# UNPACK #-} !SignalKey
    , haHandler :: SignalHandler
    }

new :: IO SignalManager
new = liftM5 SignalManager (newIORef IM.empty) newSource (newIORef True)
      (newIORef False) mallocForeignPtr

blockAllSignals :: IO ()
blockAllSignals = do
  alloca $ \blocked -> do
    -- Block all signals initially.
    sigFillSet blocked
    sigProcMask sigSetMask blocked nullPtr
  
loop :: SignalManager -> IO ()
loop SignalManager{..} =
  runInBoundThread $ do
    withForeignPtr smThread c_thread_self
    alloca $ alloca . go
 where
  go set info = do
    sigEmptySet set
    sigAddSet set sigUSR2
    handlers <- readIORef smHandlers
    forM_ (IM.keys handlers) (sigAddSet set . fromIntegral)
    sig <- sigTimedWait set info nullPtr
    woken <- if sig == sigUSR2
             then atomicModifyIORef smWakeup $ \w -> (False, w)
             else return False
    when (not woken) $
      case IM.lookup (fromIntegral sig) handlers of
        Nothing -> return ()
        Just hs -> forM_ hs $ \h -> haHandler h (haKey h)
    (`when` go set info) =<< readIORef smKeepRunning

registerHandler_ :: SignalManager -> Signal -> SignalHandler
                 -> IO (SignalKey, Bool)
registerHandler_ mgr sig hand = do
  let isig = fromIntegral sig
  when (isig <= 0 || isig >= numSignals) $
    error "registerHandler: signal number out of range"
  u <- newUnique (smUniqueSource mgr)
  atomicModifyIORef (smHandlers mgr) $ \m ->
    let !h          = Handler sk hand
        (prev, !m') = IM.insertWith (++) isig [h] m
        !sk         = SK sig u
    in (m', (sk, isNothing prev))

registerHandler :: SignalManager -> Signal -> SignalHandler -> IO SignalKey
registerHandler mgr sig hand = do
  (r, wake) <- registerHandler_ mgr sig hand
  when wake $ wakeManager mgr
  return r

unregisterHandler_ :: SignalManager -> SignalKey -> IO Bool
unregisterHandler_ mgr (SK sig key) =
    atomicModifyIORef (smHandlers mgr) $ \m ->
      let (_, !m') = IM.updateWith dropHandler isig m
      in  (m', not . IM.member isig $ m')
  where dropHandler hs = case filter ((/= key) . skUnique . haKey) hs of
                           [] -> Nothing
                           xs -> Just xs
        isig = fromIntegral sig

unregisterHandler :: SignalManager -> SignalKey -> IO ()
unregisterHandler m key = (`when` wakeManager m) =<< unregisterHandler_ m key

wakeManager :: SignalManager -> IO ()
wakeManager mgr = do
  -- XXX: This IORef usage is racy!
  writeIORef (smWakeup mgr) True
  killThread (smThread mgr) sigUSR2

numSignals :: Int
numSignals = fromIntegral c_num_signals

sigUSR2 :: Signal
sigUSR2 = #const SIGUSR2

foreign import ccall unsafe "HsEvent.h __hsevent_num_signals"
    c_num_signals :: CInt

data SigSet

instance Storable SigSet where
    sizeOf _    = #size sigset_t
    alignment _ = alignment (undefined :: CInt)

data SigInfo

instance Storable SigInfo where
    sizeOf _    = #size siginfo_t
    alignment _ = alignment (undefined :: CInt)

data TimeSpec = TimeSpec {
      _tv_sec  :: {-# UNPACK #-} !CLong
    , _tv_nsec :: {-# UNPACK #-} !CLong
    } deriving (Eq, Show)

instance Storable TimeSpec where
    sizeOf _    = #size struct timespec
    alignment _ = alignment (undefined :: CInt)

    peek ptr = liftM2 TimeSpec
               (#{peek struct timespec, tv_sec} ptr)
               (#{peek struct timespec, tv_nsec} ptr)

    poke ptr (TimeSpec s ns) = do
      #{poke struct timespec, tv_sec} ptr s
      #{poke struct timespec, tv_nsec} ptr ns

sigEmptySet :: Ptr SigSet -> IO ()
sigEmptySet = throwErrnoIfMinus1_ "sigEmptySet" . c_sigemptyset

sigFillSet :: Ptr SigSet -> IO ()
sigFillSet = throwErrnoIfMinus1_ "sigFillSet" . c_sigfillset

sigAddSet :: Ptr SigSet -> Signal -> IO ()
sigAddSet set sig = throwErrnoIfMinus1_ "sigAddSet" $
                    c_sigaddset set (fromIntegral sig)

sigTimedWait :: Ptr SigSet -> Ptr SigInfo -> Ptr TimeSpec -> IO Signal
sigTimedWait set info spec = fmap fromIntegral .
                             throwErrnoIfMinus1 "sigTimedWait" $
                             c_sigtimedwait set info spec

newtype How = How CInt
    deriving (Eq, Show)

#{enum How, How
 , sigBlock   = SIG_BLOCK
 , sigUnblock = SIG_UNBLOCK
 , sigSetMask = SIG_SETMASK
 }

sigProcMask :: How -> Ptr SigSet -> Ptr SigSet -> IO ()
sigProcMask how set oldset = throwErrnoIfMinus1_ "sigProcMask" $
                             c_sigprocmask how set oldset

data PosixThread

instance Storable PosixThread where
    sizeOf _    = #size pthread_t
    alignment _ = alignment (undefined :: CInt)

killThread :: ForeignPtr PosixThread -> Signal -> IO ()
killThread fp sig = throwErrnoIfMinus1_ "killThread" $
                    withForeignPtr fp $ \ptr -> c_kill_thread ptr sig

foreign import ccall unsafe "signal.h sigemptyset"
    c_sigemptyset :: Ptr SigSet -> IO CInt

foreign import ccall unsafe "signal.h sigfillset"
    c_sigfillset :: Ptr SigSet -> IO CInt

foreign import ccall unsafe "signal.h sigaddset"
    c_sigaddset :: Ptr SigSet -> CInt -> IO CInt

foreign import ccall safe "signal.h sigtimedwait"
    c_sigtimedwait :: Ptr SigSet -> Ptr SigInfo -> Ptr TimeSpec -> IO CInt

foreign import ccall safe "signal.h sigprocmask"
    c_sigprocmask :: How -> Ptr SigSet -> Ptr SigSet -> IO CInt

foreign import ccall unsafe "__hsevent_thread_self"
    c_thread_self :: Ptr PosixThread -> IO ()

foreign import ccall unsafe "__hsevent_kill_thread"
    c_kill_thread :: Ptr PosixThread -> Signal -> IO CInt
