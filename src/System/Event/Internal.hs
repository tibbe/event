{-# LANGUAGE ForeignFunctionInterface #-}

module System.Event.Internal where

import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)

newtype EvLoop = EvLoop ()

newtype EvIO = EvIO ()

foreign import ccall unsafe "ev.h ev_default_loop"
    c_ev_default_loop :: CInt -> IO (Ptr EvLoop)

foreign import ccall unsafe "ev.h ev_io_init"
    c_ev_io_init :: Ptr EvIO -> Ptr () -> CInt -> CInt -> IO ()
