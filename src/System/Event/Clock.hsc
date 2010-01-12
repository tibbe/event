{-# LANGUAGE ForeignFunctionInterface #-}

module System.Event.Clock (getCurrentTime) where

#include <sys/time.h>

import Foreign (Ptr, Storable(..), nullPtr, with)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt, CLong)

-- TODO: Implement this for Windows.

-- | Return the current time, in seconds since Jan. 1, 1970.
getCurrentTime :: IO Double
getCurrentTime = do
    tv <- with (CTimeval 0 0) $ \tvptr -> do
        throwErrnoIfMinus1_ "gettimeofday" (gettimeofday tvptr nullPtr)
        peek tvptr
    return $! fromIntegral (sec tv + 1000000 * usec tv)

------------------------------------------------------------------------
-- FFI binding

data CTimeval = CTimeval
    { sec  :: {-# UNPACK #-} !CLong
    , usec :: {-# UNPACK #-} !CLong
    }

instance Storable CTimeval where
    sizeOf _ = #size struct timeval
    alignment _ = alignment (undefined :: CLong)

    peek ptr = do
        sec' <- #{peek struct timeval, tv_sec} ptr
        usec' <- #{peek struct timeval, tv_usec} ptr
        return $ CTimeval sec' usec'

    poke ptr tv = do
        #{poke struct timeval, tv_sec} ptr (sec tv)
        #{poke struct timeval, tv_usec} ptr (usec tv)

foreign import ccall unsafe "sys/time.h gettimeofday" gettimeofday
    :: Ptr CTimeval -> Ptr () -> IO CInt
