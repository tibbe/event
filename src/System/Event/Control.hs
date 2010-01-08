{-# LANGUAGE ScopedTypeVariables #-}

module System.Event.Control
    (
    -- * Managing the IO manager
      ControlMessage(..)
    , Control
    , newControl
    , closeControl
    -- ** Control message reception
    , readControlMessage
    -- *** File descriptors
    , controlEventFd
    , controlReadFd
    -- ** Control message sending
    , sendWakeup
    , sendDie
    ) where

#include "EventConfig.h"

import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.Marshal (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peek, peekElemOff, poke)
import System.Posix.Internals (c_close, c_pipe, c_read, c_write,
                               setCloseOnExec, setNonBlockingFD)
import System.Posix.Types (Fd)

#if defined(HAVE_EVENTFD)
import Data.Word (Word64)
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt)
import Foreign.Marshal (allocaBytes)
import Foreign.Ptr (castPtr)
#endif

data ControlMessage = CMsgWakeup
                    | CMsgDie
    deriving (Eq, Enum, Show)

-- | The structure used to tell the IO manager thread what to do.
data Control = W {
      controlReadFd  :: {-# UNPACK #-} !Fd
    , controlWriteFd :: {-# UNPACK #-} !Fd
#if defined(HAVE_EVENTFD)
    , controlEventFd :: {-# UNPACK #-} !Fd
#endif
    }

#if !defined(HAVE_EVENTFD)
controlEventFd :: Control -> Fd
controlEventFd = controlReadFd
{-# INLINE controlEventFd #-}
#endif

-- | Create the structure (usually a pipe) used for waking up the IO
-- manager thread from another thread.
newControl :: IO Control
newControl = allocaArray 2 $ \fds -> do
  throwErrnoIfMinus1_ "createControlFds" $ c_pipe fds
  rd_end <- peekElemOff fds 0
  wr_end <- peekElemOff fds 1
#if __GLASGOW_HASKELL__ >= 611
  setNonBlockingFD wr_end True
#else
  setNonBlockingFD wr_end
#endif
  setCloseOnExec rd_end
  setCloseOnExec wr_end
#if defined(HAVE_EVENTFD)
  ev <- throwErrnoIfMinus1 "eventfd" $ c_eventfd 0 0
  setNonBlockingFD ev
  setCloseOnExec ev
#endif
  return W { controlReadFd = fromIntegral rd_end
           , controlWriteFd = fromIntegral wr_end
#if defined(HAVE_EVENTFD)
           , controlEventFd = fromIntegral ev
#endif
           }

-- | Close the control structure used by the IO manager thread.
closeControl :: Control -> IO ()
closeControl w = do
  _ <- c_close . fromIntegral . controlReadFd $ w
  _ <- c_close . fromIntegral . controlWriteFd $ w
#if defined(HAVE_EVENTFD)
  _ <- c_close . fromIntegral . controlEventFd $ w
#endif
  return ()

readControlMessage :: Control -> Fd -> IO ControlMessage
readControlMessage ctrl fd
#if defined(HAVE_EVENTFD)
    | fd == controlEventFd ctrl = allocaBytes 8 $ \p -> do
                    throwErrnoIfMinus1_ "readControlMessage" $
                      c_read (fromIntegral fd) p 8
                    return CMsgWakeup
#endif
    | otherwise = alloca $ \p -> do
                    throwErrnoIfMinus1_ "readControlMessage" $
                      c_read (fromIntegral fd) p 1
                    (toEnum . fromIntegral) `fmap` peek p

sendWakeup :: Control -> IO ()
#if defined(HAVE_EVENTFD)
sendWakeup c = alloca $ \p -> do
  poke p (1 :: Word64)
  throwErrnoIfMinus1_ "writeControlMessage" $
    c_write (fromIntegral (controlEventFd c)) (castPtr p) 8
#else
sendWakeup c = sendControlMessage c CMsgWakeup
#endif

sendDie :: Control -> IO ()
sendDie c = sendControlMessage c CMsgDie

sendControlMessage :: Control -> ControlMessage -> IO ()
sendControlMessage w msg = alloca $ \p -> do
  poke p (fromIntegral (fromEnum msg))
  throwErrnoIfMinus1_ "sendControlMessage" $
    c_write (fromIntegral (controlWriteFd w)) p 1

#if defined(HAVE_EVENTFD)
foreign import ccall unsafe "sys/eventfd.h eventfd"
   c_eventfd :: CInt -> CInt -> IO CInt
#endif
