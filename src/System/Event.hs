{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module System.Event
    ( -- * Types
      EventLoop,

      -- * Creation
      new,

      -- * Registering interest in I/O events
      Event(..),
      Callback,
      setFD,

      -- * Registering timeout callbacks
      setTimeout,
      updateTimeout,
      clearTimeout,

      -- * Event loop
      loop
    ) where


------------------------------------------------------------------------
-- Imports

import           Control.Monad (sequence_)
import           Data.IntMap as IM
import           Data.IORef
import           Data.Maybe (maybe)
import           Data.Time.Clock ( NominalDiffTime
                                 , UTCTime
                                 , addUTCTime
                                 , diffUTCTime
                                 , getCurrentTime)
import           Data.Unique
import           System.Posix.Types (Fd(..))
                
import           System.Event.Internal (Backend, Event(..), Timeout(..))
import qualified System.Event.Internal as I
import qualified System.Event.TimeoutTable as TT

#ifdef BACKEND_KQUEUE
import qualified System.Event.KQueue as KQueue
#elif  BACKEND_EPOLL
import qualified System.Event.EPoll  as EPoll
#else
# error not implemented for this operating system
#endif


------------------------------------------------------------------------
-- Types

-- | Vector of callbacks indexed by file descriptor.
type Callbacks = IntMap ([Event] -> IO ())

-- FIXME: choose a quicker time representation than UTCTime? We'll be calling
-- "getCurrentTime" a lot.
type TimeRep         = UTCTime
type TimeoutKey      = Unique
type TimeoutCallback = IO ()
type TimeoutTable    = TT.TimeoutTable TimeRep TimeoutKey TimeoutCallback

-- | The event loop state.
data EventLoop = forall a. Backend a => EventLoop
    { _elBackend      :: !a                    -- ^ Backend
    , _elIOCallbacks  :: !(IORef Callbacks)    -- ^ I/O callbacks
    , _elTimeoutTable :: !(IORef TimeoutTable) -- ^ Timeout table
    }

------------------------------------------------------------------------
-- Creation

-- | Create a new event loop.
new :: IO EventLoop
new = do
#ifdef BACKEND_KQUEUE
    be <- KQueue.new
#elif  BACKEND_EPOLL
    be <- EPoll.new
#endif
    cbs <- newIORef empty
    tms <- newIORef TT.empty
    return $ EventLoop be cbs tms

------------------------------------------------------------------------
-- Event loop

-- | Start handling events.  This function never returns.
loop :: EventLoop -> IO ()
loop el@(EventLoop be _ tt) = do
    now <- getCurrentTime
    go now

  where
    --------------------------------------------------------------------------
    go now = do
        timeout <- mkTimeout now
        reason  <- I.poll be timeout ioCallback

        now'    <- getCurrentTime

        case reason of
          I.TimedOut -> timeoutCallback now'
          _          -> return ()

        go now'

    --------------------------------------------------------------------------
    inMs :: NominalDiffTime -> Maybe Timeout
    inMs d =
        if v <= 0 then Nothing else Just $ Timeout v
      where
        v = floor (1000 * d)

    --------------------------------------------------------------------------
    timeoutCallback = onTimeoutEvent el
    ioCallback      = onFdEvent el

    --------------------------------------------------------------------------
    mkTimeout now = do
        tt' <- readIORef tt

        let mbOldest = TT.findOldest tt'

        -- if there are expired items in the timeout table then we need to run
        -- the callback now; normally this would be handled within I.poll but
        -- it could happen if e.g. one of the timeout callbacks took a long
        -- time
        maybe (return Forever)
              (\(tm,_,_) -> maybe (timeoutCallback now >> mkTimeout now)
                                  return
                                  (inMs $ diffUTCTime tm now))
              mbOldest


------------------------------------------------------------------------
-- Registering interest in events

-- | Callback invoked on I/O events.
type Callback = [Event] -> IO ()

-- | @set el cb fd evs@ registers interest in the events @evs@ on the
-- file descriptor @fd@.  @cb@ is called for each event that occurs.
setFD :: EventLoop -> Callback -> Fd -> [Event] -> IO ()
setFD (EventLoop be cbs _) cb fd evs = do
    atomicModifyIORef cbs $ \c -> (IM.insert (fromIntegral fd) cb c, ())
    I.set be (fromIntegral fd) evs
    -- TODO: uncomment once wakeup is implemented in the backends

    -- I.wakeup be


------------------------------------------------------------------------
-- Registering timeout callbacks

setTimeout :: EventLoop -> Int -> TimeoutCallback -> IO TimeoutKey
setTimeout (EventLoop _ _ tt) ms cb = do
    now <- getCurrentTime
    let expTime = addUTCTime (1000 * fromIntegral ms) now
    key <- newUnique

    atomicModifyIORef tt $ \tab -> (TT.insert expTime key cb tab, ())
    -- I.wakeup be
    return key


clearTimeout :: EventLoop -> TimeoutKey -> IO ()
clearTimeout (EventLoop _ _ tt) key = do
    atomicModifyIORef tt $ \tab -> (TT.delete key tab, ())
    -- I.wakeup be
    return ()


updateTimeout :: EventLoop -> TimeoutKey -> Int -> IO ()
updateTimeout (EventLoop _ _ tt) key ms = do
    now <- getCurrentTime
    let expTime = addUTCTime (1000 * fromIntegral ms) now

    atomicModifyIORef tt $ \tab -> (TT.update key expTime tab, ())
    -- I.wakeup be
    return ()


------------------------------------------------------------------------
-- Utilities

-- | Call the callback corresponding to the given file descriptor.
onFdEvent :: EventLoop -> Fd -> [Event] -> IO ()
onFdEvent (EventLoop _ cbs' _) fd evs = do
    cbs <- readIORef cbs'
    case IM.lookup (fromIntegral fd) cbs of
        Just cb -> cb evs
        Nothing -> return ()  -- TODO: error?


onTimeoutEvent :: EventLoop -> TimeRep -> IO ()
onTimeoutEvent (EventLoop _ _ tt) now = do
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

