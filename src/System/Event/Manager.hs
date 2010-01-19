{-# LANGUAGE BangPatterns, CPP, ExistentialQuantification, RecordWildCards #-}
module System.Event.Manager
    ( -- * Types
      EventManager

      -- * Creation
    , new
    , newWith

      -- * Running
    , loop
    , wakeManager

      -- * Registering interest in I/O events
    , Event
    , evtRead
    , evtWrite
    , IOCallback
    , FdRegistration
    , registerFd_
    , registerFd
    , unregisterFd__
    , unregisterFd_
    , unregisterFd
    , fdWasClosed

      -- * Registering interest in timeout events
    , TimeoutCallback
    , TimeoutKey
    , registerTimeout
    , updateTimeout
    , clearTimeout
    ) where

#include "EventConfig.h"

------------------------------------------------------------------------
-- Imports

import Control.Monad (forM_, when)
import Data.IORef
import Data.Monoid (mconcat, mempty)
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
    , _fdCallback :: {-# UNPACK #-} !IOCallback
    }

-- | Callback invoked on I/O events.
type IOCallback = Fd -> Event -> IO ()

type TimeRep         = Double
type TimeoutKey      = Unique

-- | Callback invoked on timeout events.
type TimeoutCallback = IO ()

-- | The event manager state.
data EventManager = forall a. Backend a => EventManager
    { emBackend      :: !a
    , emFds          :: !(IORef (IM.IntMap [FdData]))
    , emTimeouts     :: !(IORef (Q.PSQ TimeoutCallback))
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
  _ <- registerFd_ mgr (handleControlEvent mgr) (controlReadFd ctrl) evtRead
  _ <- registerFd_ mgr (handleControlEvent mgr) (wakeupReadFd ctrl) evtRead
  return mgr

------------------------------------------------------------------------
-- Event loop

-- | Start handling events.  This function loops until told to stop.
loop :: EventManager -> IO ()
loop mgr@EventManager{..} = go =<< getCurrentTime
  where
    go now = do
        timeout <- mkTimeout now
        I.poll emBackend timeout (onFdEvent mgr)

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
            Just (Q.E _ t _, _) ->
                return $! Timeout (ceiling $ (t - now) * 1000)

------------------------------------------------------------------------
-- Registering interest in I/O events

-- | A file descriptor registration cookie.
data FdRegistration = FdRegistration {
      _regFd     :: {-# UNPACK #-} !Fd
    , _regUnique :: {-# UNPACK #-} !Unique
    } deriving (Eq, Ord)

-- | Register interest in the given events, without waking the event
-- manager thread.  The 'Bool' return value indicates whether the
-- event manager needs to be woken.
registerFd_ :: EventManager -> IOCallback -> Fd -> Event
            -> IO (FdRegistration, Bool)
registerFd_ EventManager{..} cb fd evs = do
  u <- newUnique emUniqueSource
  let fd' = fromIntegral fd
  (oldEvs, newEvs) <- atomicModifyIORef emFds $ \f ->
    case IM.insertLookupWithKey (const (++)) fd' [FdData u evs cb] f of
      (Nothing,   newMap) -> (newMap, (mempty, evs))
      (Just prev, newMap) -> (newMap, pairEvents prev newMap fd')
  let modify = oldEvs /= newEvs
  when modify $ I.modifyFd emBackend fd oldEvs newEvs
  let !reg = FdRegistration fd u
  return (reg, modify)
{-# INLINE registerFd_ #-}

-- | @registerFd mgr cb fd evs@ registers interest in the events @evs@
-- on the file descriptor @fd@.  @cb@ is called for each event that
-- occurs.  Returns a cookie that can be handed to 'unregisterFd'.
registerFd :: EventManager -> IOCallback -> Fd -> Event -> IO FdRegistration
registerFd mgr cb fd evs = do
  (r, wake) <- registerFd_ mgr cb fd evs
  when wake $ wakeManager mgr
  return r
{-# INLINE registerFd #-}

-- | Wake up the event manager.
wakeManager :: EventManager -> IO ()
wakeManager mgr = sendWakeup (emControl mgr)

eventsOf :: [FdData] -> Event
eventsOf = mconcat . map fdEvents

pairEvents :: [FdData] -> IM.IntMap [FdData] -> Int -> (Event, Event)
pairEvents prev m fd = let !l = eventsOf prev
                           !r = case IM.lookup fd m of
                                  Nothing  -> mempty
                                  Just fds -> eventsOf fds
                       in (l, r)

-- | Drop a previous file descriptor registration, without modifying
-- the event manager's back end.  The return value contains the old
-- and new values of the events watched for this file descriptor.
unregisterFd__ :: EventManager -> FdRegistration -> IO (Event, Event)
unregisterFd__ mgr (FdRegistration fd u) = do
  let dropReg _ cbs = case filter ((/= u) . fdUnique) cbs of
                        []   -> Nothing
                        cbs' -> Just cbs'
      fd' = fromIntegral fd
  v@(!_, !_) <- atomicModifyIORef (emFds mgr) $ \f ->
    case IM.updateLookupWithKey dropReg fd' f of
      (Nothing,   _)      -> (f,      (mempty, mempty))
      (Just prev, newMap) -> (newMap, pairEvents prev newMap fd')
  return v

-- | Drop a previous file descriptor registration, without waking the
-- event manager thread.  The return value indicates whether the event
-- manager needs to be woken.
unregisterFd_ :: EventManager -> FdRegistration -> IO Bool
unregisterFd_ mgr@EventManager{..} reg@(FdRegistration fd _) = do
  (oldEvs, newEvs) <- unregisterFd__ mgr reg
  let modify = oldEvs /= newEvs
  when modify $ I.modifyFd emBackend fd oldEvs newEvs
  return $! modify

-- | Drop a previous file descriptor registration.
unregisterFd :: EventManager -> FdRegistration -> IO ()
unregisterFd mgr reg = do
  wake <- unregisterFd_ mgr reg
  when wake $ wakeManager mgr

-- | Notify the event manager that a file descriptor has been closed.
fdWasClosed :: EventManager -> Fd -> IO ()
fdWasClosed mgr fd = do
  oldEvs <- atomicModifyIORef (emFds mgr) $ \f ->
    case IM.updateLookupWithKey (\ _ _ -> Nothing) (fromIntegral fd) f of
      (Nothing,  _)       -> (f,      mempty)
      (Just fds, !newMap) -> (newMap, eventsOf fds)
  when (oldEvs /= mempty) $ wakeManager mgr

------------------------------------------------------------------------
-- Registering interest in timeout events

-- | Register a timeout in the given number of milliseconds.
registerTimeout :: EventManager -> Int -> TimeoutCallback -> IO TimeoutKey
registerTimeout mgr ms cb = do
    now <- getCurrentTime
    let expTime = fromIntegral ms / 1000.0 + now
    key <- newUnique (emUniqueSource mgr)

    atomicModifyIORef (emTimeouts mgr) $ \q ->
        let !q' = Q.insert key expTime cb q in (q', ())
    wakeManager mgr
    return key

clearTimeout :: EventManager -> TimeoutKey -> IO ()
clearTimeout mgr key = do
    atomicModifyIORef (emTimeouts mgr) $ \q ->
        let !q' = Q.delete key q in (q', ())
    wakeManager mgr

updateTimeout :: EventManager -> TimeoutKey -> Int -> IO ()
updateTimeout mgr key ms = do
    now <- getCurrentTime
    let expTime = fromIntegral ms / 1000.0 + now

    atomicModifyIORef (emTimeouts mgr) $ \q ->
        let !q' = Q.adjust (const expTime) key q in (q', ())
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
        Nothing  -> return ()
