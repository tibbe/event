{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}

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
      clearTimeout,

      -- * Event loop
      loop
    ) where

------------------------------------------------------------------------
-- Imports

import Control.Monad (liftM3, sequence_)
import Data.IntMap as IM
import Data.IORef
import Data.Maybe (maybe)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime,
                        getCurrentTime)
import Data.Unique
import System.Posix.Types (Fd)

import System.Event.Internal (Backend, Event, evtRead, evtWrite, Timeout(..), wmWakeup)
import qualified System.Event.Internal as I
import qualified System.Event.TimeoutTable as TT

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
    { _elBackend      :: !a                     -- ^ Backend
    , _elIOCallbacks  :: !(IORef (IntMap IOCallback))   -- ^ I/O callbacks
    , _elTimeoutTable :: !(IORef TimeoutTable)  -- ^ Timeout table
    }

------------------------------------------------------------------------
-- Creation

-- | Create a new event manager.
new :: IO EventManager
new = liftM3 EventManager Backend.new (newIORef empty) (newIORef TT.empty)

------------------------------------------------------------------------
-- Event loop

-- | Start handling events.  This function never returns.
loop :: EventManager -> IO ()
loop mgr@(EventManager be _ tt) = go =<< getCurrentTime
  where
    go now = do
        timeout <- mkTimeout now
        reason  <- I.poll be timeout ioCallback

        now'    <- getCurrentTime

        case reason of
          I.TimedOut -> timeoutCallback now'
          _          -> return ()

        go now'

    inMs :: NominalDiffTime -> Maybe Timeout
    inMs d =
        if v <= 0 then Nothing else Just $ Timeout v
      where
        v = floor (1000 * d)

    timeoutCallback = onTimeoutEvent mgr
    ioCallback      = onFdEvent mgr

    mkTimeout now = do
        tt' <- readIORef tt

        let mbOldest = TT.findOldest tt'

        -- If there are expired items in the timeout table then we
        -- need to run the callback now; normally this would be
        -- handled within I.poll but it could happen if e.g. one of
        -- the timeout callbacks took a long time
        maybe (return Forever)
              (\(tm,_,_) -> maybe (timeoutCallback now >> mkTimeout now)
                                  return
                                  (inMs $ diffUTCTime tm now))
              mbOldest

------------------------------------------------------------------------
-- Registering interest in I/O events

-- | @registerFd mgr cb fd evs@ registers interest in the events @evs@
-- on the file descriptor @fd@.  @cb@ is called for each event that
-- occurs.
registerFd :: EventManager -> IOCallback -> Fd -> Event -> IO ()
registerFd (EventManager be cbs _) cb fd evs = do
    atomicModifyIORef cbs $ \c -> (IM.insert (fromIntegral fd) cb c, ())
    I.set be (fromIntegral fd) evs
    I.wakeup be wmWakeup

------------------------------------------------------------------------
-- Registering interest in timeout events

registerTimeout :: EventManager -> Int -> TimeoutCallback -> IO TimeoutKey
registerTimeout (EventManager be _ tt) ms cb = do
    now <- getCurrentTime
    let expTime = addUTCTime (1000 * fromIntegral ms) now
    key <- newUnique

    atomicModifyIORef tt $ \tab -> (TT.insert expTime key cb tab, ())
    I.wakeup be wmWakeup
    return key

clearTimeout :: EventManager -> TimeoutKey -> IO ()
clearTimeout (EventManager be _ tt) key = do
    atomicModifyIORef tt $ \tab -> (TT.delete key tab, ())
    I.wakeup be wmWakeup

updateTimeout :: EventManager -> TimeoutKey -> Int -> IO ()
updateTimeout (EventManager be _ tt) key ms = do
    now <- getCurrentTime
    let expTime = addUTCTime (1000 * fromIntegral ms) now

    atomicModifyIORef tt $ \tab -> (TT.update key expTime tab, ())
    I.wakeup be wmWakeup

------------------------------------------------------------------------
-- Utilities

-- | Call the callback corresponding to the given file descriptor.
onFdEvent :: EventManager -> Fd -> Event -> IO ()
onFdEvent (EventManager _ cbs' _) fd evs = do
    cbs <- readIORef cbs'
    case IM.lookup (fromIntegral fd) cbs of
        Just cb -> cb fd evs
        Nothing -> return ()  -- TODO: error?

onTimeoutEvent :: EventManager -> TimeRep -> IO ()
onTimeoutEvent (EventManager _ _ tt) now = do
    touts <- atomicModifyIORef tt grabExpired
    sequence_ touts

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
