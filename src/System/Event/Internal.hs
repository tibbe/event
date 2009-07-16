module System.Event.Internal where

import Foreign.C.Types (CInt)

-- | An I/O event.
data Event = Read   -- ^ The file descriptor is ready for reading
           | Write  -- ^ The file descriptor is ready for writing

-- | Event notification backend.
class Backend a where
    -- | Create a new backend.
    new :: IO a

    -- | Poll backend for new events.  The provide callback is called
    -- once per file descriptor with new events.
    poll :: a
         -> (CInt -> [Event] -> IO ())  -- Callback
         -> IO ()

    -- | Register interest in the given events on the given file
    -- descriptor.
    set :: a
        -> CInt     -- File descriptor
        -> [Event]  -- Events to watch for
        -> IO ()
