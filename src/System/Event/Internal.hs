module System.Event.Internal
    (
    -- * Core types
      Backend(..)
    , Event
    , evtRead
    , evtWrite
    , eventIs
    , throwErrnoIfMinus1NoRetry
    , Timeout(..)
    ) where

import Data.Bits ((.|.), (.&.))
import Data.List (foldl')
import Data.Monoid (Monoid(..))
import Foreign.C.Error (eINTR, getErrno, throwErrno)
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
{-# INLINE evtRead #-}

evtWrite :: Event
evtWrite = Event 2
{-# INLINE evtWrite #-}

eventIs :: Event -> Event -> Bool
eventIs (Event a) (Event b) = a .&. b /= 0

instance Show Event where
    show e = show . filter (not . null) $
             [evtRead `so` "evtRead", evtWrite `so` "evtWrite"]
        where ev `so` disp | e `eventIs` ev = disp
                           | otherwise      = ""

instance Monoid Event where
    mempty  = evtNothing
    mappend = evtCombine
    mconcat = evtConcat

evtCombine :: Event -> Event -> Event
evtCombine (Event a) (Event b) = Event (a .|. b)
{-# INLINE evtCombine #-}

evtConcat :: [Event] -> Event
evtConcat = foldl' evtCombine evtNothing
{-# INLINE evtConcat #-}

-- | A type alias for timeouts, specified in milliseconds.
data Timeout = Timeout {-# UNPACK #-} !CInt
             | Forever
               deriving (Show)

-- | Event notification backend.
class Backend a where
    -- | Create a new backend.
    new :: IO a

    -- | Poll backend for new events.  The provided callback is called
    -- once per file descriptor with new events.
    poll :: a                          -- ^ backend state
         -> Timeout                    -- ^ timeout in milliseconds
         -> (Fd -> Event -> IO ())     -- ^ I/O callback
         -> IO ()

    -- | Register, modify, or unregister interest in the given events
    -- on the given file descriptor.
    modifyFd :: a
             -> Fd       -- ^ file descriptor
             -> Event    -- ^ old events to watch for ('mempty' for new)
             -> Event    -- ^ new events to watch for ('mempty' to delete)
             -> IO ()

-- | Throw an 'IOError' corresponding to the current value of
-- 'getErrno' if the result value of the 'IO' action is -1 and
-- 'getErrno' is not 'eINTR'.  If the result value is -1 and
-- 'getErrno' returns 'eINTR' 0 is returned.  Otherwise the result
-- value is returned.
throwErrnoIfMinus1NoRetry :: Num a => String -> IO a -> IO a
throwErrnoIfMinus1NoRetry loc f = do
    res <- f
    if res == -1
        then do
            err <- getErrno
            if err == eINTR then return 0 else throwErrno loc
        else return res
