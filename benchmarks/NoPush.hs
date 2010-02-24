{-# INCLUDE <sys/socket.h> #-}
{-# INCLUDE <netinet/tcp.h> #-}
{-# INCLUDE <netinet/in.h> #-}
{-# LINE 1 "NoPush.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "NoPush.hsc" #-}

module NoPush (setNoPush) where


{-# LINE 6 "NoPush.hsc" #-}

{-# LINE 7 "NoPush.hsc" #-}

{-# LINE 8 "NoPush.hsc" #-}

import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import Foreign.Storable (sizeOf)
import Network.Socket (Socket(..))

noPush :: CInt

{-# LINE 18 "NoPush.hsc" #-}
noPush = 4
{-# LINE 19 "NoPush.hsc" #-}

{-# LINE 24 "NoPush.hsc" #-}

setNoPush :: Socket -> Bool -> IO ()
setNoPush _ _ | noPush == 0 = return ()
setNoPush (MkSocket fd _ _ _ _) onOff = do
  let v = if onOff then 1 else 0
  with v $ \ptr ->
    throwErrnoIfMinus1_ "setNoPush" $
      c_setsockopt fd (6) noPush ptr (fromIntegral (sizeOf v))
{-# LINE 32 "NoPush.hsc" #-}

foreign import stdcall unsafe "setsockopt"
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt
