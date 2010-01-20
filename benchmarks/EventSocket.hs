{-# LANGUAGE ForeignFunctionInterface #-}

-- | Socket functions using System.Event instead of GHC's I/O manager.
module EventSocket
    (
      accept
    , recv
    , send
    , sendAll
    ) where

import Control.Concurrent (newMVar, readMVar)
import Control.Monad (liftM, when)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (createAndTrim)
import Data.Word (Word8)
import qualified Data.ByteString as B
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.C.Types (CChar, CInt, CSize)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.Error (eINTR, eWOULDBLOCK, eAGAIN, getErrno, throwErrno)
import GHC.IOBase (IOErrorType(..), IOException(..))
import Network.Socket (SockAddr, Socket(..), SocketStatus(..))
import Network.Socket.Internal
import System.Event.Thread
import System.Posix.Internals

recv :: Socket -> Int -> IO ByteString
recv (MkSocket s _ _ _ _) nbytes
    | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.ByteString.recv")
    | otherwise   = createAndTrim nbytes $ recvInner s nbytes

recvInner :: CInt -> Int -> Ptr Word8 -> IO Int
recvInner s nbytes ptr = do
    len <- throwErrnoIfMinus1Retry_repeatOnBlock "recv"
           (threadWaitRead (fromIntegral s)) $
           c_recv s (castPtr ptr) (fromIntegral nbytes) 0{-flags-}
    case fromIntegral len of
         (-1) -> do errno <- getErrno
                    if errno == eINTR
                       then recvInner s nbytes ptr
                       else throwErrno "Network.Socket.ByteString.recv"
         n -> return n

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

accept :: Socket -> IO (Socket, SockAddr)
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
        return (MkSocket new_sock family stype protocol new_status, addr)

{-# SPECIALISE
    throwErrnoIfMinus1Retry_mayBlock
         :: String -> IO CInt -> IO CInt -> IO CInt #-}
throwErrnoIfMinus1Retry_mayBlock :: Num a => String -> IO a -> IO a -> IO a
throwErrnoIfMinus1Retry_mayBlock name on_block act = do
    res <- act
    if res == -1
        then do
            err <- getErrno
            if err == eINTR
                then throwErrnoIfMinus1Retry_mayBlock name on_block act
                else if err == eWOULDBLOCK || err == eAGAIN
                        then on_block
                        else throwErrno name
        else return res

throwErrnoIfMinus1Retry_repeatOnBlock :: Num a => String -> IO b -> IO a -> IO a
throwErrnoIfMinus1Retry_repeatOnBlock name on_block act = do
  throwErrnoIfMinus1Retry_mayBlock name (on_block >> repeat) act
  where repeat = throwErrnoIfMinus1Retry_repeatOnBlock name on_block act

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = IOError Nothing
                                    InvalidArgument
                                    loc "non-positive length" Nothing

foreign import ccall unsafe "sys/socket.h accept"
    c_accept :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> IO CInt

foreign import ccall unsafe "sys/socket.h send"
    c_send :: CInt -> Ptr a -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "sys/socket.h recv"
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
