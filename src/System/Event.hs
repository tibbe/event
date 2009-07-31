{-# LANGUAGE CPP, ExistentialQuantification, ForeignFunctionInterface #-}

module System.Event
    ( -- * Types
      EventLoop,

      -- * Creation
      new,

      -- * Registering interest in events
      Event(..),
      Callback,
      set,

      -- * Event loop
      loop
    ) where

import Control.Monad.ST
import Foreign.C.Types (CInt)

import System.Event.Internal (Backend, Event(..))

import qualified System.Event.Internal as I
import qualified System.Event.Vector as V

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
type Callbacks = V.Vector RealWorld ([Event] -> IO ())

-- | The event loop state.
data EventLoop = forall a. Backend a => EventLoop
    !a  -- Backend
    {-# UNPACK #-} !Callbacks

------------------------------------------------------------------------
-- Creation

-- | Create a new event loop.
new :: IO EventLoop
new = do
#ifdef BACKEND_KQUEUE
    be <- KQueue.new  -- TODO: Detect backend to use.
#endif
    cbs <- stToIO V.empty
    return $ EventLoop be cbs

------------------------------------------------------------------------
-- Event loop

-- | Start handling events.  This function never returns.
loop :: EventLoop -> IO ()
loop (EventLoop be cbs) = loop'
    where loop' = I.poll be (onFdEvent cbs) >> loop'

------------------------------------------------------------------------
-- Registering interest in events

-- | Callback invoked on I/O events.
type Callback = [Event] -> IO ()

-- | @set el cb fd evs@ registers interest in the events @evs@ on the
-- file descriptor @fd@.  @cb@ is called for each event that occurs.
set :: EventLoop -> Callback -> CInt -> [Event] -> IO ()
set (EventLoop be fds) cb fd evs = do
    stToIO $ do V.reserve fds (fromIntegral $ fd - 1)
                V.unsafeWrite fds (fromIntegral fd) cb
    I.set be fd evs

------------------------------------------------------------------------
-- Utilities

-- | Call the callback corresponding to the given file descriptor.
onFdEvent :: Callbacks -> CInt -> [Event] -> IO ()
onFdEvent cbs fd evs =
    stToIO (V.unsafeRead cbs (fromIntegral fd)) >>= \f -> f evs
