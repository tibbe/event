{-# LANGUAGE BangPatterns, CPP, ExistentialQuantification, RecordWildCards #-}
module System.Event.Manager
    ( -- * Types
      EventManager,

      -- * Creation
      new,
      newWith,

      -- * Running
      loop,
      wakeManager,

      -- * Registering interest in I/O events
      Event,
      evtRead,
      evtWrite,
      IOCallback,
      FdRegistration,
      registerFd_,
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

import Control.Monad (forM_, when)
import Data.IORef
import Data.Monoid (mempty)
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
#endif
#if defined(HAVE_EPOLL)
import qualified System.Event.EPoll  as EPoll
#endif
#if defined(HAVE_POLL)
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
handleControlEvent mgr fd _evt = do
  msg <- readControlMessage (emControl mgr) fd
  case msg of
    CMsgWakeup -> return ()
    CMsgDie    -> writeIORef (emKeepRunning mgr) False

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

-- | Create a new event manager.
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
  return mgr

------------------------------------------------------------------------
-- Event loop

-- | Start handling events.  This function loops until told to stop.
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

-- | Register interest in the given events, without waking the event
-- manager thread.
registerFd_ :: EventManager -> IOCallback -> Fd -> Event -> IO FdRegistration
registerFd_ EventManager{..} cb fd evs = do
  u <- newUnique emUniqueSource
  let fd' = fromIntegral fd
  atomicModifyIORef emFds $ \c ->
      (IM.insertWith (++) fd' [FdData u evs cb] c, ())
  -- TODO: fix up the API to pass the old and new event masks for this
  -- Fd to the back end
  I.modifyFd emBackend fd mempty evs
  return $! FdRegistration fd u

-- | @registerFd mgr cb fd evs@ registers interest in the events @evs@
-- on the file descriptor @fd@.  @cb@ is called for each event that
-- occurs.  Returns a cookie that can be handed to 'unregisterFd'.
registerFd :: EventManager -> IOCallback -> Fd -> Event -> IO FdRegistration
registerFd mgr cb fd evs = do
  r <- registerFd_ mgr cb fd evs
  wakeManager mgr
  return r

-- | Wake up the event manager.
wakeManager :: EventManager -> IO ()
wakeManager mgr = sendWakeup (emControl mgr)

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
registerTimeout mgr ms cb = do
    now <- getCurrentTime
    let expTime = fromIntegral (1000 * ms) + now
    key <- newUnique (emUniqueSource mgr)

    atomicModifyIORef (emTimeouts mgr) $ \q ->
        (Q.insert key expTime cb q, ())
    wakeManager mgr
    return key

clearTimeout :: EventManager -> TimeoutKey -> IO ()
clearTimeout mgr key = do
    atomicModifyIORef (emTimeouts mgr) $ \q -> (Q.delete key q, ())
    wakeManager mgr

updateTimeout :: EventManager -> TimeoutKey -> Int -> IO ()
updateTimeout mgr key ms = do
    now <- getCurrentTime
    let expTime = fromIntegral (1000 * ms) + now

    atomicModifyIORef (emTimeouts mgr) $ \q ->
        (Q.adjust (const expTime) key q, ())
    wakeManager mgr

------------------------------------------------------------------------
-- Utilities

-- | Call the callbacks corresponding to the given file descriptor.
onFdEvent :: EventManager -> Fd -> Event -> IO ()
onFdEvent mgr fd evs = do
    fds <- readIORef (emFds mgr)
    case IM.lookup (fromIntegral fd) fds of
        Just cbs -> forM_ cbs $ \(FdData _ ev cb) ->
                      when (evs `I.eventIs` ev) $ cb fd evs
        Nothing  -> return ()  -- TODO: error?
