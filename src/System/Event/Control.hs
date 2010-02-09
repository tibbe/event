{-# LANGUAGE CPP, ForeignFunctionInterface, ScopedTypeVariables #-}

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
    , controlReadFd
    , wakeupReadFd
    -- ** Control message sending
    , sendWakeup
    , sendDie
    -- * Utilities
    , setNonBlockingFD
    ) where

#include "EventConfig.h"

import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt)
import Foreign.Marshal (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peek, peekElemOff, poke)
import System.Posix.Internals (c_close, c_pipe, c_read, c_write,
                               setCloseOnExec, setNonBlockingFD)
import System.Posix.Types (Fd)

#if defined(HAVE_EVENTFD)
import Data.Word (Word64)
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.Ptr (castPtr)
#else
import Control.Monad (when)
import Foreign.C.Error (eAGAIN, eWOULDBLOCK, getErrno, throwErrno)
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
#else
    , wakeupReadFd   :: {-# UNPACK #-} !Fd
    , wakeupWriteFd  :: {-# UNPACK #-} !Fd
#endif
    } deriving (Show)

#if defined(HAVE_EVENTFD)
wakeupReadFd :: Control -> Fd
wakeupReadFd = controlEventFd
{-# INLINE wakeupReadFd #-}
#endif

setNonBlock :: CInt -> IO ()
setNonBlock fd =
#if __GLASGOW_HASKELL__ >= 611
  setNonBlockingFD fd True
#else
  setNonBlockingFD fd
#endif

-- | Create the structure (usually a pipe) used for waking up the IO
-- manager thread from another thread.
newControl :: IO Control
newControl = allocaArray 2 $ \fds -> do
  let createPipe = do
        throwErrnoIfMinus1_ "pipe" $ c_pipe fds
        rd <- peekElemOff fds 0
        wr <- peekElemOff fds 1
        -- The write end must be non-blocking, since we may need to
        -- poke the event manager from a signal handler.
        setNonBlock wr
        setCloseOnExec rd
        setCloseOnExec wr
        return (fromIntegral rd, fromIntegral wr)
  (ctrl_rd, ctrl_wr) <- createPipe
#if defined(HAVE_EVENTFD)
  ev <- throwErrnoIfMinus1 "eventfd" $ c_eventfd 0 0
  setNonBlock ev
  setCloseOnExec ev
#else
  (wake_rd, wake_wr) <- createPipe
#endif
  return W { controlReadFd  = ctrl_rd
           , controlWriteFd = ctrl_wr
#if defined(HAVE_EVENTFD)
           , controlEventFd = fromIntegral ev
#else
           , wakeupReadFd   = wake_rd
           , wakeupWriteFd  = wake_wr
#endif
           }

-- | Close the control structure used by the IO manager thread.
closeControl :: Control -> IO ()
closeControl w = do
  _ <- c_close . fromIntegral . controlReadFd $ w
  _ <- c_close . fromIntegral . controlWriteFd $ w
#if defined(HAVE_EVENTFD)
  _ <- c_close . fromIntegral . controlEventFd $ w
#else
  _ <- c_close . fromIntegral . wakeupReadFd $ w
  _ <- c_close . fromIntegral . wakeupWriteFd $ w
#endif
  return ()

readControlMessage :: Control -> Fd -> IO ControlMessage
readControlMessage ctrl fd
    | fd == wakeupReadFd ctrl = allocaBytes wakeupBufferSize $ \p -> do
                    throwErrnoIfMinus1_ "readWakeupMessage" $
                      c_read (fromIntegral fd) p (fromIntegral wakeupBufferSize)
                    return CMsgWakeup
    | otherwise = alloca $ \p -> do
                    throwErrnoIfMinus1_ "readControlMessage" $
                      c_read (fromIntegral fd) p 1
                    (toEnum . fromIntegral) `fmap` peek p
  where wakeupBufferSize = 
#if defined(HAVE_EVENTFD)
            8
#else
            4096
#endif

sendWakeup :: Control -> IO ()
#if defined(HAVE_EVENTFD)
sendWakeup c = alloca $ \p -> do
  poke p (1 :: Word64)
  throwErrnoIfMinus1_ "sendWakeup" $
    c_write (fromIntegral (controlEventFd c)) (castPtr p) 8
#else
sendWakeup c = do
  n <- sendMessage (wakeupWriteFd c) CMsgWakeup
  case undefined of
    _ | n /= -1   -> return ()
      | otherwise -> do
                   errno <- getErrno
                   when (errno /= eAGAIN && errno /= eWOULDBLOCK) $
                     throwErrno "sendWakeup"
#endif

sendDie :: Control -> IO ()
sendDie c = throwErrnoIfMinus1_ "sendDie" $
            sendMessage (controlWriteFd c) CMsgDie

sendMessage :: Fd -> ControlMessage -> IO Int
sendMessage fd msg = alloca $ \p -> do
  poke p (fromIntegral (fromEnum msg))
  fromIntegral `fmap` c_write (fromIntegral fd) p 1

#if defined(HAVE_EVENTFD)
foreign import ccall unsafe "sys/eventfd.h eventfd"
   c_eventfd :: CInt -> CInt -> IO CInt
#endif
