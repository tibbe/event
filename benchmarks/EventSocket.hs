{-# LANGUAGE ForeignFunctionInterface #-}

-- | Socket functions using System.Event instead of GHC's I/O manager.
module EventSocket where

import Control.Concurrent (newMVar, readMVar)
import Control.Monad (liftM, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import Network.Socket (SockAddr, Socket(..), SocketStatus(..))
import Network.Socket.Internal
import System.Event.Thread
import System.Posix.Internals

------------------------------------------------------------------------
-- Sending

-- | Send data to the socket.  The socket must be connected to a
-- remote socket.  Returns the number of bytes sent. Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Number of bytes sent
send (MkSocket s _ _ _ _) xs =
    unsafeUseAsCStringLen xs $ \(str, len) ->
    liftM fromIntegral $
        throwSocketErrorIfMinus1RetryMayBlock "send"
        (threadWaitWrite (fromIntegral s)) $
        c_send s str (fromIntegral len) 0

foreign import ccall unsafe "sys/socket.h send"
    c_send :: CInt -> Ptr a -> CSize -> CInt -> IO CInt

-- | Send data to the socket.  The socket must be connected to a
-- remote socket.  Unlike 'send', this function continues to send data
-- until either all data has been sent or an error occurs.  On error,
-- an exception is raised, and there is no way to determine how much
-- data, if any, was successfully sent.
sendAll :: Socket      -- ^ Connected socket
        -> ByteString  -- ^ Data to send
        -> IO ()
sendAll sock bs = do
    sent <- send sock bs
    when (sent < B.length bs) $ sendAll sock (B.drop sent bs)

------------------------------------------------------------------------
-- Accepting

accept :: Socket -> IO Socket
accept sock@(MkSocket s family stype protocol status) = do
    currentStatus <- readMVar status
    let sz = sizeOfSockAddrByFamily family
    allocaBytes sz $ \ sockaddr -> do
        with (fromIntegral sz) $ \ ptr_len -> do
        new_sock <- throwSocketErrorIfMinus1RetryMayBlock "accept"
                    (threadWaitRead (fromIntegral s)) $
                    c_accept s sockaddr ptr_len
        setNonBlockingFD new_sock
        addr <- peekSockAddr sockaddr
        new_status <- newMVar Connected
        return (MkSocket new_sock family stype protocol new_status)

foreign import ccall unsafe "accept"
    c_accept :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> IO CInt
