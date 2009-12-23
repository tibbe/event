module System.Event.Internal where

import Foreign.C.Types (CInt)
import System.Posix.Types (Fd)

-- | An I/O event.
data Event = Read   -- ^ The file descriptor is ready to be read
           | Write  -- ^ The file descriptor is ready to be written to

-- | A type alias for timeouts
data Timeout = Timeout CInt
             | Forever

-- | Indicates whether poll returned because of activity or timeout
data Result = Activity | TimedOut

-- | Event notification backend.
class Backend a where
    -- | Create a new backend.
    new :: IO a

    -- | Poll backend for new events.  The provided callback is called
    -- once per file descriptor with new events.
    poll :: a                          -- ^ backend state 
         -> Timeout                    -- ^ timeout in milliseconds
         -> (Fd -> [Event] -> IO ())   -- ^ I/O callback
         -> IO Result

    -- | Register interest in the given events on the given file
    -- descriptor.
    set :: a
        -> Fd       -- ^ file descriptor
        -> [Event]  -- ^ events to watch for
        -> IO ()

    -- | This should cause the underlying polling mechanism to unblock. An
    -- | example of how to do this is provided by the GHC runtime system: when
    -- | you create the Backend, create a pipe and register interest in the
    -- | read end.
    wakeup :: a -> IO ()
