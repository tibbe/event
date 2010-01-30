{-# LANGUAGE BangPatterns, CPP, EmptyDataDecls, ForeignFunctionInterface,
    RecordWildCards #-}

module System.Event.Signal
    (
      SignalManager
    , Signal
    , SignalHandler
    , SignalKey(skSignal)
    , new
    , loop
    , registerHandler_
    , registerHandler
    , unregisterHandler_
    , unregisterHandler
    , wakeManager
    ) where

#include "EventConfig.h"

#if defined(HAVE_SIGNAL_H)
# include <signal.h>
#endif

import Control.Monad (forM_, liftM2, liftM4, when)
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import Foreign.C.Error (throwErrnoIfMinus1, throwErrnoIfMinus1_)
import Foreign.C.Types (CInt, CLong)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))
import System.Event.Unique (Unique, UniqueSource, newUnique, newSource)
import qualified System.Event.IntMap as IM

data SignalManager = SignalManager {
      smHandlers     :: {-# UNPACK #-} !(IORef (IM.IntMap [Handler]))
    , smUniqueSource :: {-# UNPACK #-} !UniqueSource
    , smKeepRunning  :: {-# UNPACK #-} !(IORef Bool)
    , smWakeup       :: {-# UNPACK #-} !(IORef Bool)
    }

type SignalHandler = SignalKey -> IO ()

type Signal = Int

data SignalKey = SK {
      skSignal :: {-# UNPACK #-} !Signal
    , skUnique :: {-# UNPACK #-} !Unique
    } deriving (Eq)

data Handler = Handler {
      haKey     :: {-# UNPACK #-} !SignalKey
    , haHandler :: SignalHandler
    }

new :: IO SignalManager
new = liftM4 SignalManager (newIORef IM.empty) newSource (newIORef True)
      (newIORef False)

loop :: SignalManager -> IO ()
loop mgr@SignalManager{..} =
    alloca $ \set ->
      alloca $ \info -> go set info
 where
  go set info = do
    sigEmptySet set
    handlers <- readIORef smHandlers
    forM_ (IM.keys handlers) $ sigAddSet set
    sig <- sigTimedWait set info nullPtr
    woken <- if sig == sigusr2
             then atomicModifyIORef smWakeup $ \w -> (False, w)
             else return False
    when (not woken) $
      case IM.lookup sig handlers of
        Nothing -> return ()
        Just hs -> forM_ hs $ \h -> haHandler h (haKey h)
    (`when` go set info) =<< readIORef smKeepRunning

registerHandler_ :: SignalManager -> Signal -> SignalHandler
                 -> IO (SignalKey, Bool)
registerHandler_ mgr sig hand = do
  when (sig <= 0 || sig >= numSignals) $
    error "registerHandler: signal number out of range"
  u <- newUnique (smUniqueSource mgr)
  atomicModifyIORef (smHandlers mgr) $ \m ->
    let !h          = Handler sk hand
        (prev, !m') = IM.insertWith (++) sig [h] m
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
      let (_, !m') = IM.updateWith dropHandler sig m
      in  (m', not . IM.member sig $ m')
  where dropHandler hs = case filter ((/= key) . skUnique . haKey) hs of
                           [] -> Nothing
                           xs -> Just xs

unregisterHandler :: SignalManager -> SignalKey -> IO ()
unregisterHandler m key = (`when` wakeManager m) =<< unregisterHandler_ m key

wakeManager :: SignalManager -> IO ()
wakeManager mgr = do
  -- TODO: think hard about whether this IORef usage is racy
  writeIORef (smWakeup mgr) True
  raise sigusr2

numSignals :: Int
numSignals = fromIntegral c_num_signals

sigusr2 :: Int
sigusr2 = fromIntegral c_SIGUSR2

foreign import ccall unsafe "HsEvent.h __hsevent_num_signals"
    c_num_signals :: CInt

foreign import ccall unsafe "HsEvent.h __hsevent_sigusr2"
    c_SIGUSR2 :: CInt

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

sigAddSet :: Ptr SigSet -> Signal -> IO ()
sigAddSet set sig = throwErrnoIfMinus1_ "sigAddSet" $
                    c_sigaddset set (fromIntegral sig)

sigTimedWait :: Ptr SigSet -> Ptr SigInfo -> Ptr TimeSpec -> IO Signal
sigTimedWait set info spec = fmap fromIntegral .
                             throwErrnoIfMinus1 "sigTimedWait" $
                             c_sigtimedwait set info spec

raise :: Signal -> IO ()
raise sig = throwErrnoIfMinus1_ "raise" $ c_raise (fromIntegral sig)

foreign import ccall unsafe "signal.h sigemptyset"
    c_sigemptyset :: Ptr SigSet -> IO CInt

foreign import ccall unsafe "signal.h sigaddset"
    c_sigaddset :: Ptr SigSet -> CInt -> IO CInt

foreign import ccall safe "signal.h sigtimedwait"
    c_sigtimedwait :: Ptr SigSet -> Ptr SigInfo -> Ptr TimeSpec -> IO CInt

foreign import ccall unsafe "raise"
  c_raise :: CInt -> IO CInt
