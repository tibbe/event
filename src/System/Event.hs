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

------------------------------------------------------------------------
-- Imports

import Control.Concurrent (forkIO)
import Control.Monad (when)
import Data.IORef
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime,
                        getCurrentTime)
import System.Posix.Types (Fd)

import System.Event.Internal (Backend, Event, evtRead, evtWrite, Timeout(..))
import qualified System.Event.Internal as I
import qualified System.Event.TimeoutTable as TT
import System.Event.Control
import qualified System.Event.IntMap as IM
import System.Event.Unique

#if defined(BACKEND_KQUEUE)
import qualified System.Event.KQueue as Backend
#elif defined(BACKEND_EPOLL)
import qualified System.Event.EPoll  as Backend
#else
# error not implemented for this operating system
#endif

------------------------------------------------------------------------
-- Types

-- | Callback invoked on I/O events.
type IOCallback = Fd -> Event -> IO ()

-- FIXME: choose a quicker time representation than UTCTime? We'll be calling
-- "getCurrentTime" a lot.
type TimeRep         = UTCTime
type TimeoutKey      = Unique
type TimeoutCallback = IO ()
type TimeoutTable    = TT.TimeoutTable TimeRep TimeoutKey TimeoutCallback

-- | The event manager state.
data EventManager = forall a. Backend a => EventManager
    { emBackend      :: !a                     -- ^ Backend
    , emIOCallbacks  :: !(IORef (IM.IntMap IOCallback))   -- ^ I/O callbacks
    , emTimeoutTable :: !(IORef TimeoutTable)  -- ^ Timeout table
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
  timeouts <- newIORef TT.empty
  ctrl <- newControl
  run <- newIORef True
  us <- newSource
  let mgr = EventManager { emBackend = be
                         , emIOCallbacks = iocbs
                         , emTimeoutTable = timeouts
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
        reason  <- I.poll emBackend timeout ioCallback

        now'    <- getCurrentTime

        case reason of
          I.TimedOut -> timeoutCallback now'
          _          -> return ()

        keepRunning <- readIORef emKeepRunning
        when keepRunning $
          go now'

    inMs :: NominalDiffTime -> Maybe Timeout
    inMs d =
        if v <= 0 then Nothing else Just $ Timeout v
      where
        v = floor (1000 * d)

    timeoutCallback = onTimeoutEvent mgr
    ioCallback      = onFdEvent mgr

    mkTimeout now = do
        tt' <- readIORef emTimeoutTable

        -- If there are expired items in the timeout table then we
        -- need to run the callback now; normally this would be
        -- handled within I.poll but it could happen if e.g. one of
        -- the timeout callbacks took a long time
        case TT.findOldest tt' of
          Nothing       -> return Forever
          Just (tm,_,_) -> case inMs (diffUTCTime tm now) of
                             Nothing -> timeoutCallback now >> mkTimeout now
                             Just t  -> return t

------------------------------------------------------------------------
-- Registering interest in I/O events

registerFd_ :: EventManager -> IOCallback -> Fd -> Event -> IO ()
registerFd_ EventManager{..} cb fd evs = do
  atomicModifyIORef emIOCallbacks $ \c -> (IM.insert (fromIntegral fd) cb c, ())
  I.set emBackend (fromIntegral fd) evs

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
    let expTime = addUTCTime (1000 * fromIntegral ms) now
    key <- newUnique emUniqueSource

    atomicModifyIORef emTimeoutTable $ \tab ->
        (TT.insert expTime key cb tab, ())
    sendWakeup emControl
    return key

clearTimeout :: EventManager -> TimeoutKey -> IO ()
clearTimeout EventManager{..} key = do
    atomicModifyIORef emTimeoutTable $ \tab -> (TT.delete key tab, ())
    sendWakeup emControl

updateTimeout :: EventManager -> TimeoutKey -> Int -> IO ()
updateTimeout EventManager{..} key ms = do
    now <- getCurrentTime
    let expTime = addUTCTime (1000 * fromIntegral ms) now

    atomicModifyIORef emTimeoutTable $ \tab -> (TT.update key expTime tab, ())
    sendWakeup emControl

------------------------------------------------------------------------
-- Utilities

-- | Call the callback corresponding to the given file descriptor.
onFdEvent :: EventManager -> Fd -> Event -> IO ()
onFdEvent EventManager{..} fd evs = do
    cbs <- readIORef emIOCallbacks
    case IM.lookup (fromIntegral fd) cbs of
        Just cb -> cb fd evs
        Nothing -> return ()  -- TODO: error?

onTimeoutEvent :: EventManager -> TimeRep -> IO ()
onTimeoutEvent EventManager{..} now =
    sequence_ =<< atomicModifyIORef emTimeoutTable grabExpired

  where
    grabExpired :: TimeoutTable -> (TimeoutTable, [TimeoutCallback])
    grabExpired table = go [] table

    go l table =
        case TT.findOldest table of
          Nothing      -> (table,l)
          Just (t,k,c) -> if expired t
                            then let !table' = TT.delete k table
                                 in go (c:l) table'
                            else (table, l)

    expired t = diffUTCTime now t >= 0
