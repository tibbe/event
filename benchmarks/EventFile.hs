{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- | File functions using System.Event instead of GHC's I/O manager.
module EventFile
    (
      read
    ) where

import Control.Concurrent (modifyMVar_, newMVar)
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
import Foreign.C.Error (Errno(..), eINPROGRESS, eINTR, eWOULDBLOCK, eAGAIN,
                        errnoToIOError, getErrno, throwErrno)
#if __GLASGOW_HASKELL__ < 612
import GHC.IOBase (IOErrorType(..))
#else
import GHC.IO.Exception (IOErrorType(..))
#endif
import Network.Socket hiding (accept, connect, recv, send)
#if defined(USE_GHC_IO_MANAGER)
import GHC.Conc (threadWaitRead)
#else
import System.Event.Thread (threadWaitRead)
#endif
import System.IO.Error (ioeSetErrorString, mkIOError)
import System.Posix.Internals (c_read)
import System.Posix.Types (Fd)
import Prelude hiding (read)

read :: Fd -> Int -> IO ByteString
read fd nbytes
    | nbytes <= 0 = ioError (mkInvalidReadArgError "read")
    | otherwise   = createAndTrim nbytes $ readInner fd nbytes

readInner :: Fd -> Int -> Ptr Word8 -> IO Int
readInner fd nbytes ptr = do
    len <- throwErrnoIfMinus1Retry_repeatOnBlock "read"
           (threadWaitRead (fromIntegral fd)) $
           c_read (fromIntegral fd) (castPtr ptr) (fromIntegral nbytes)
    case fromIntegral len of
         (-1) -> do errno <- getErrno
                    if errno == eINTR
                       then readInner fd nbytes ptr
                       else throwErrno "read"
         n -> return n

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

mkInvalidReadArgError :: String -> IOError
mkInvalidReadArgError loc = ioeSetErrorString (mkIOError
				               InvalidArgument
                                               loc Nothing Nothing)
                            "non-positive length"
