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
    -- * Managing the IO manager
    , WakeupMessage
    , Wakeup
    , createWakeup
    , closeWakeup
    , readWakeupMessage
    , writeWakeupMessage
    , wakeupReadFd
    , wakeupWriteFd
    -- ** Wakeup message types
    , wmWakeup
    , wmDie
    ) where

import Control.Monad (liftM)
import Data.Bits ((.|.), (.&.))
import Data.Monoid (Monoid(..))
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CChar, CInt)
import Foreign.Marshal (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peek, peekElemOff, poke)
import System.Posix.Internals (c_close, c_pipe, c_read, c_write,
                               setCloseOnExec, setNonBlockingFD)
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

newtype WakeupMessage = WM CChar

wmWakeup :: WakeupMessage
wmWakeup = WM 1

wmDie :: WakeupMessage
wmDie = WM 2

-- | The structure used to tell the IO manager thread what to do.
data Wakeup = W {
      wakeupReadFd :: {-# UNPACK #-} !Fd
    , wakeupWriteFd :: {-# UNPACK #-} !Fd
    }

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

    -- | This should cause the underlying polling mechanism to
    -- unblock. See 'createWakeup' and friends for a possible
    -- implementation.
    wakeup :: a -> WakeupMessage -> IO ()

-- | Create the structure (usually a pipe) used for waking up the IO
-- manager thread from another thread.
createWakeup :: IO Wakeup
createWakeup = allocaArray 2 $ \fds -> do
  throwErrnoIfMinus1_ "createWakeupFds" $ c_pipe fds
  rd_end <- peekElemOff fds 0
  wr_end <- peekElemOff fds 1
#if __GLASGOW_HASKELL__ >= 611
  setNonBlockingFD wr_end True
#else
  setNonBlockingFD wr_end
#endif
  setCloseOnExec rd_end
  setCloseOnExec wr_end
  return W { wakeupReadFd = fromIntegral rd_end
           , wakeupWriteFd = fromIntegral wr_end }

-- | Close the wakeup structure used by the IO manager thread.
closeWakeup :: Wakeup -> IO ()
closeWakeup (W rd_end wr_end) = do
  throwErrnoIfMinus1_ "closeWakeupFds" $ c_close (fromIntegral rd_end)
  throwErrnoIfMinus1_ "closeWakeupFds" $ c_close (fromIntegral wr_end)

readWakeupMessage :: Wakeup -> IO WakeupMessage
readWakeupMessage (W fd _) = alloca $ \p -> do
  throwErrnoIfMinus1_ "readWakeupMessage" $ c_read (fromIntegral fd) p 1
  WM `liftM` peek p

writeWakeupMessage :: Wakeup -> WakeupMessage -> IO ()
writeWakeupMessage (W _ fd) (WM m) = alloca $ \p -> do
  poke p m
  throwErrnoIfMinus1_ "writeWakeupMessage" $ c_write (fromIntegral fd) p 1
