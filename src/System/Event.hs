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
      FdRegistration,
      registerFd,
      unregisterFd,

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
import Control.Monad (forM_, when)
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
import qualified System.Event.KQueue as KQueue
#elif defined(HAVE_EPOLL)
import qualified System.Event.EPoll  as EPoll
#elif defined(HAVE_POLL)
import qualified System.Event.Poll   as Poll
#else
# error not implemented for this operating system
#endif

------------------------------------------------------------------------
-- Types

data FdData = FdData {
      fdUnique   :: {-# UNPACK #-} !Unique
    , fdEvents   :: {-# UNPACK #-} !Event
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
    , emFds          :: !(IORef (IM.IntMap [FdData]))
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

#if defined(HAVE_KQUEUE)
newDefaultBackend :: IO KQueue.Backend
newDefaultBackend = KQueue.new
#elif defined(HAVE_EPOLL)
newDefaultBackend :: IO EPoll.Backend
newDefaultBackend = EPoll.new
#elif defined(HAVE_POLL)
newDefaultBackend :: IO Poll.Backend
newDefaultBackend = Poll.new
#else
newDefaultBackend :: IO a
newDefaultBackend = error "no back end for this platform"
#endif

-- | Create and run a new event manager.
new :: IO EventManager
new = newWith =<< newDefaultBackend

newWith :: I.Backend b => b -> IO EventManager
newWith be = do
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
  _ <- registerFd_ mgr (handleControlEvent mgr) read_fd evtRead
  when (read_fd /= event_fd) $ do
    _ <- registerFd_ mgr (handleControlEvent mgr) event_fd evtRead
    return ()
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

-- | A file descriptor registration cookie.
data FdRegistration = FdRegistration {
      _regFd     :: {-# UNPACK #-} !Fd
    , _regUnique :: {-# UNPACK #-} !Unique
    }

registerFd_ :: EventManager -> IOCallback -> Fd -> Event -> IO FdRegistration
registerFd_ EventManager{..} cb fd evs = do
  u <- newUnique emUniqueSource
  let fd' = fromIntegral fd
  atomicModifyIORef emFds $ \c ->
      (IM.insertWith (++) fd' [FdData u evs cb] c, ())
  -- TODO: fix up the API to pass the old and new event masks for this
  -- Fd to the back end
  I.registerFd emBackend fd evs
  return $! FdRegistration fd u

-- | @registerFd mgr cb fd evs@ registers interest in the events @evs@
-- on the file descriptor @fd@.  @cb@ is called for each event that
-- occurs.  Returns a cookie that can be handed to 'unregisterFd'.
registerFd :: EventManager -> IOCallback -> Fd -> Event -> IO FdRegistration
registerFd mgr cb fd evs = do
  r <- registerFd_ mgr cb fd evs
  sendWakeup (emControl mgr)
  return r

-- | Drop a previous file descriptor registration.
unregisterFd :: EventManager -> FdRegistration -> IO ()
unregisterFd mgr (FdRegistration fd u) = do
  let f cbs = case filter ((/= u) . fdUnique) cbs of
                []   -> Nothing
                cbs' -> Just cbs'
      fd' = fromIntegral fd
  stillInUse <- atomicModifyIORef (emFds mgr) $ \c -> let c' = IM.update f fd' c
                                                      in (c', IM.member fd' c')
  -- TODO: unregister with back end if no longer in use
  return ()

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

-- | Call the callbacks corresponding to the given file descriptor.
onFdEvent :: EventManager -> Fd -> Event -> IO ()
onFdEvent EventManager{..} fd evs = do
    cbs <- readIORef emFds
    case IM.lookup (fromIntegral fd) cbs of
        Just cbs -> forM_ cbs $ \(FdData _ ev cb) ->
                      when (evs `I.eventIs` ev) $ cb fd evs
        Nothing  -> return ()  -- TODO: error?
