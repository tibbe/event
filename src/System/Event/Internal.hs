module System.Event.Internal
    (
    -- * Core types
      Backend(..)
    , Event
    , evtRead
    , evtWrite
    , eventIs
    , Result(..)
    , Timeout(..)
    ) where

import Data.Bits ((.|.), (.&.))
import Data.Monoid (Monoid(..))
import Foreign.C.Types (CInt)
import System.Posix.Types (Fd)

-- | An I/O event.
newtype Event = Event Int
    deriving (Eq)

evtNothing :: Event
evtNothing = Event 0
{-# INLINE evtNothing #-}

evtRead :: Event
evtRead = Event 1

evtWrite :: Event
evtWrite = Event 2

eventIs :: Event -> Event -> Bool
eventIs (Event a) (Event b) = a .&. b /= 0

instance Show Event where
    show e | e `eventIs` evtRead  = "evtRead"
           | e `eventIs` evtWrite = "evtWrite"
           | otherwise            = error "show: illegal value"

instance Monoid Event where
    mempty  = evtNothing
    mappend = evtCombine

evtCombine :: Event -> Event -> Event
evtCombine (Event a) (Event b) = Event (a .|. b)
{-# INLINE evtCombine #-}

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
         -> (Fd -> Event -> IO ())     -- ^ I/O callback
         -> IO Result

    -- | Register interest in the given events on the given file
    -- descriptor.
    set :: a
        -> Fd       -- ^ file descriptor
        -> Event    -- ^ events to watch for
        -> IO ()
