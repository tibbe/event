{-# LANGUAGE BangPatterns, CPP, ExistentialQuantification, RecordWildCards #-}
module System.Event
    ( -- * Types
      EventManager,

      -- * Creation
      new,

      -- * Registering interest in I/O events
      Event,
      evtRead,
      evtWrite,
      IOCallback,
      registerFd,

      -- * Registering interest in timeout events
      TimeoutCallback,
      registerTimeout,
      updateTimeout,
      clearTimeout
    ) where

#include "EventConfig.h"

------------------------------------------------------------------------
-- Imports

import Control.Concurrent (forkIO)
import Control.Monad (when)
import Data.IORef
import System.Posix.Types (Fd)

import System.Event.Clock
import System.Event.Internal (Backend, Event, evtRead, evtWrite, Timeout(..))
import qualified System.Event.Internal as I
import qualified System.Event.PSQ as Q
import System.Event.Control
import qualified System.Event.IntMap as IM
import System.Event.Unique

#if defined(HAVE_KQUEUE)
import qualified System.Event.KQueue as Backend
#elif defined(HAVE_EPOLL)
import qualified System.Event.EPoll  as Backend
#elif defined(HAVE_POLL)
import qualified System.Event.Poll   as Backend
#else
# error not implemented for this operating system
#endif

------------------------------------------------------------------------
-- Types

data FdData = FdData {
      fdEvents   :: {-# UNPACK #-} !Event
    , fdCallback :: {-# UNPACK #-} !IOCallback
    }

-- | Callback invoked on I/O events.
type IOCallback = Fd -> Event -> IO ()

type TimeRep         = Double
type TimeoutKey      = Unique

-- | Callback invoked on timeout events.
type TimeoutCallback = IO ()

-- | The event manager state.
data EventManager = forall a. Backend a => EventManager
    { emBackend      :: !a                     -- ^ Backend
    , emFds          :: !(IORef (IM.IntMap FdData))
    , emTimeouts     :: !(IORef (Q.PSQ TimeoutCallback))  -- ^ Timeouts
    , emKeepRunning  :: !(IORef Bool)
    , emUniqueSource :: !UniqueSource
    , emControl      :: !Control
    }

------------------------------------------------------------------------
-- Creation

handleControlEvent :: EventManager -> Fd -> Event -> IO ()
handleControlEvent EventManager{..} fd _evt = do
  msg <- readControlMessage emControl fd
  case msg of
    CMsgWakeup -> return ()
    CMsgDie    -> writeIORef emKeepRunning False

-- | Create and run a new event manager.
new :: IO EventManager
new = do
  be <- Backend.new
  iocbs <- newIORef IM.empty
  timeouts <- newIORef Q.empty
  ctrl <- newControl
  run <- newIORef True
  us <- newSource
  let mgr = EventManager { emBackend = be
                         , emFds = iocbs
                         , emTimeouts = timeouts
                         , emKeepRunning = run
                         , emUniqueSource = us
                         , emControl = ctrl
                         }
      read_fd  = controlReadFd ctrl
      event_fd = controlEventFd ctrl
  registerFd_ mgr (handleControlEvent mgr) read_fd evtRead
  when (read_fd /= event_fd) $
    registerFd_ mgr (handleControlEvent mgr) event_fd evtRead
  _ <- forkIO $ loop mgr
  return mgr

------------------------------------------------------------------------
-- Event loop

-- | Start handling events.  This function returns when told to.
loop :: EventManager -> IO ()
loop mgr@EventManager{..} = go =<< getCurrentTime
  where
    go now = do
        timeout <- mkTimeout now
        _ <- I.poll emBackend timeout (onFdEvent mgr)

        now'    <- getCurrentTime

        keepRunning <- readIORef emKeepRunning
        when keepRunning $
          go now'

    -- | Call all expired timer callbacks and return the time to the
    -- next timeout.
    mkTimeout :: TimeRep -> IO Timeout
    mkTimeout now = do
        (expired, q') <- atomicModifyIORef emTimeouts $ \q ->
            let res@(_, q') = Q.atMost now q in (q', res)
        sequence_ $ map Q.value expired
        case Q.minView q' of
            Nothing             -> return Forever
            Just (Q.E _ t _, _) -> return $! Timeout (floor (t - now))

------------------------------------------------------------------------
-- Registering interest in I/O events

registerFd_ :: EventManager -> IOCallback -> Fd -> Event -> IO ()
registerFd_ EventManager{..} cb fd evs = do
  atomicModifyIORef emFds $ \c ->
      (IM.insert (fromIntegral fd) (FdData evs cb) c, ())
  I.registerFd emBackend (fromIntegral fd) evs

-- | @registerFd mgr cb fd evs@ registers interest in the events @evs@
-- on the file descriptor @fd@.  @cb@ is called for each event that
-- occurs.
registerFd :: EventManager -> IOCallback -> Fd -> Event -> IO ()
registerFd mgr cb fd evs = do
  registerFd_ mgr cb fd evs
  sendWakeup (emControl mgr)

------------------------------------------------------------------------
-- Registering interest in timeout events

registerTimeout :: EventManager -> Int -> TimeoutCallback -> IO TimeoutKey
registerTimeout EventManager{..} ms cb = do
    now <- getCurrentTime
    let expTime = fromIntegral (1000 * ms) + now
    key <- newUnique emUniqueSource

    atomicModifyIORef emTimeouts $ \q ->
        (Q.insert key expTime cb q, ())
    sendWakeup emControl
    return key

clearTimeout :: EventManager -> TimeoutKey -> IO ()
clearTimeout EventManager{..} key = do
    atomicModifyIORef emTimeouts $ \q -> (Q.delete key q, ())
    sendWakeup emControl

updateTimeout :: EventManager -> TimeoutKey -> Int -> IO ()
updateTimeout EventManager{..} key ms = do
    now <- getCurrentTime
    let expTime = fromIntegral (1000 * ms) + now

    atomicModifyIORef emTimeouts $ \q ->
        (Q.adjust (const expTime) key q, ())
    sendWakeup emControl

------------------------------------------------------------------------
-- Utilities

-- | Call the callback corresponding to the given file descriptor.
onFdEvent :: EventManager -> Fd -> Event -> IO ()
onFdEvent EventManager{..} fd evs = do
    cbs <- readIORef emFds
    case IM.lookup (fromIntegral fd) cbs of
        Just (FdData _ cb) -> cb fd evs
        Nothing            -> return ()  -- TODO: error?
