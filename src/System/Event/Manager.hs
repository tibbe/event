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
    , FdKey(keyFd)
    , registerFd_
    , registerFd
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

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Monad (forM_, when)
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef, writeIORef)
import Data.Monoid (mconcat, mempty)
import System.Posix.Types (Fd)

import System.Event.Clock (getCurrentTime)
import System.Event.Internal (Backend, Event, evtRead, evtWrite, Timeout(..))
import qualified System.Event.Internal as I
import qualified System.Event.PSQ as Q
import System.Event.Control
import qualified System.Event.IntMap as IM
import System.Event.Unique (Unique, UniqueSource, newSource, newUnique)

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
      fdKey       :: {-# UNPACK #-} !FdKey
    , fdEvents    :: {-# UNPACK #-} !Event
    , _fdCallback :: {-# UNPACK #-} !IOCallback
    }

-- | A file descriptor registration cookie.
data FdKey = FdKey {
      keyFd     :: {-# UNPACK #-} !Fd
    , keyUnique :: {-# UNPACK #-} !Unique
    } deriving (Eq, Show)

-- | Callback invoked on I/O events.
type IOCallback = FdKey -> Event -> IO ()

type TimeRep         = Double
newtype TimeoutKey   = TK Unique
    deriving (Eq)

-- | Callback invoked on timeout events.
type TimeoutCallback = IO ()

-- | The event manager state.
data EventManager = forall a. Backend a => EventManager
    { emBackend      :: !a
    , emFds          :: !(MVar (IM.IntMap [FdData]))
    , emTimeouts     :: !(IORef (Q.PSQ TimeoutCallback))
    , emKeepRunning  :: !(IORef Bool)
    , emUniqueSource :: !UniqueSource
    , emControl      :: !Control
    }

------------------------------------------------------------------------
-- Creation

handleControlEvent :: EventManager -> FdKey -> Event -> IO ()
handleControlEvent mgr reg _evt = do
  msg <- readControlMessage (emControl mgr) (keyFd reg)
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
  iofds <- newMVar IM.empty
  timeouts <- newIORef Q.empty
  ctrl <- newControl
  run <- newIORef True
  us <- newSource
  let mgr = EventManager { emBackend = be
                         , emFds = iofds
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
loop mgr@EventManager{..} = go
  where
    go = do
      timeout <- mkTimeout
      I.poll emBackend timeout (onFdEvent mgr)
      flip when go =<< readIORef emKeepRunning

    -- | Call all expired timer callbacks and return the time to the
    -- next timeout.
    mkTimeout :: IO Timeout
    mkTimeout = do
        now <- getCurrentTime
        (expired, q') <- atomicModifyIORef emTimeouts $ \q ->
            let res@(_, q') = Q.atMost now q in (q', res)
        sequence_ $ map Q.value expired
        case Q.minView q' of
            Nothing             -> return Forever
            Just (Q.E _ t _, _) ->
                return $! Timeout (max 0 . ceiling $ (t - now) * 1000)

------------------------------------------------------------------------
-- Registering interest in I/O events

-- | Register interest in the given events, without waking the event
-- manager thread.  The 'Bool' return value indicates whether the
-- event manager ought to be woken.
registerFd_ :: EventManager -> IOCallback -> Fd -> Event
            -> IO (FdKey, Bool)
registerFd_ EventManager{..} cb fd evs = do
  u <- newUnique emUniqueSource
  modifyMVar emFds $ \oldMap -> do
    let fd'  = fromIntegral fd
        reg  = FdKey fd u
        !fdd = FdData reg evs cb
        (!newMap, (oldEvs, newEvs)) =
            case IM.insertWith (++) fd' [fdd] oldMap of
              (Nothing,   n) -> (n, (mempty, evs))
              (Just prev, n) -> (n, pairEvents prev newMap fd')
        modify = oldEvs /= newEvs
    when modify $ I.modifyFd emBackend fd oldEvs newEvs
    return (newMap, (reg, modify))
{-# INLINE registerFd_ #-}

-- | @registerFd mgr cb fd evs@ registers interest in the events @evs@
-- on the file descriptor @fd@.  @cb@ is called for each event that
-- occurs.  Returns a cookie that can be handed to 'unregisterFd'.
registerFd :: EventManager -> IOCallback -> Fd -> Event -> IO FdKey
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

-- | Drop a previous file descriptor registration, without waking the
-- event manager thread.  The return value indicates whether the event
-- manager ought to be woken.
unregisterFd_ :: EventManager -> FdKey -> IO Bool
unregisterFd_ EventManager{..} (FdKey fd u) =
  modifyMVar emFds $ \oldMap -> do
    let dropReg cbs = case filter ((/= u) . keyUnique . fdKey) cbs of
                          []   -> Nothing
                          cbs' -> Just cbs'
        fd' = fromIntegral fd
        (!newMap, (oldEvs, newEvs)) =
            case IM.updateWith dropReg fd' oldMap of
              (Nothing,   _)    -> (oldMap, (mempty, mempty))
              (Just prev, newm) -> (newm, pairEvents prev newm fd')
        modify = oldEvs /= newEvs
    when modify $ I.modifyFd emBackend fd oldEvs newEvs
    return (newMap, modify)

-- | Drop a previous file descriptor registration.
unregisterFd :: EventManager -> FdKey -> IO ()
unregisterFd mgr reg = do
  wake <- unregisterFd_ mgr reg
  when wake $ wakeManager mgr

-- | Notify the event manager that a file descriptor has been closed.
fdWasClosed :: EventManager -> Fd -> IO ()
fdWasClosed mgr fd =
  modifyMVar_ (emFds mgr) $ \oldMap ->
    case IM.delete (fromIntegral fd) oldMap of
      (Nothing,  _)       -> return oldMap
      (Just fds, !newMap) -> do
        when (eventsOf fds /= mempty) $ wakeManager mgr
        return newMap

------------------------------------------------------------------------
-- Registering interest in timeout events

-- | Register a timeout in the given number of milliseconds.
registerTimeout :: EventManager -> Int -> TimeoutCallback -> IO TimeoutKey
registerTimeout mgr ms cb = do
  key <- newUnique (emUniqueSource mgr)
  if ms <= 0 then cb
    else do
      now <- getCurrentTime
      let expTime = fromIntegral ms / 1000.0 + now

      atomicModifyIORef (emTimeouts mgr) $ \q ->
          let !q' = Q.insert key expTime cb q in (q', ())
      wakeManager mgr
  return $! TK key

clearTimeout :: EventManager -> TimeoutKey -> IO ()
clearTimeout mgr (TK key) = do
    atomicModifyIORef (emTimeouts mgr) $ \q ->
        let !q' = Q.delete key q in (q', ())
    wakeManager mgr

updateTimeout :: EventManager -> TimeoutKey -> Int -> IO ()
updateTimeout mgr (TK key) ms = do
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
  fds <- readMVar (emFds mgr)
  case IM.lookup (fromIntegral fd) fds of
      Just cbs -> forM_ cbs $ \(FdData reg ev cb) ->
                    when (evs `I.eventIs` ev) $ cb reg evs
      Nothing  -> return ()
