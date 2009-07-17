{-# LANGUAGE ForeignFunctionInterface #-}

module System.Event.EPoll where

import Foreign.C.Error
import Foreign.C.Types
import Foreign.Ptr

import qualified System.Event.Internal as E

#include <sys/epoll.h>

------------------------------------------------------------------------
-- FFI binding

newtype EPollFd = EPollFd { unEPollFd :: CInt }
    deriving (Eq, Show)

newtype ControlOp = ControlOp { unControlOp :: CInt }
    deriving (Eq, Show)

#{enum ControlOp, ControlOp
 , controlOpAdd    = EPOLL_CTL_ADD
 , controlOpModify = EPOLL_CTL_MOD
 , controlOpDelete = EPOLL_CTL_DEL
 }

newtype EventType = EventType { unEventType :: CUInt }
    deriving (Eq, Show)

#{enum EventType, EventType
 , eventTypeReadyForRead           = EPOLLIN
 , eventTypeReadyForWrite          = EPOLLOUT
 , eventTypePeerClosedConnection   = EPOLLRDHUP
 , eventTypeUrgentDataReadyForRead = EPOLLPRI
 , eventTypeError                  = EPOLLERR
 , eventTypeHangup                 = EPOLLHUP
 , eventTypeEdgeTriggered          = EPOLLET
 , eventTypeOneShot                = EPOLLONESHOT
 }

foreign import ccall unsafe "epoll.h epoll_create"
    c_epoll_create :: IO CInt

foreign import ccall unsafe "epoll.h epoll_ctl"
    c_epoll_ctl :: CInt -> CInt -> CInt -> Ptr Event -> IO CInt

foreign import ccall unsafe "epoll.h epoll_wait"
    c_epoll_wait :: CInt -> Ptr Event -> CInt -> CInt -> IO CInt

epollCreate :: IO EPollFd
epollCreate = 
    fmap EPollFd throwErrnoIfMinus1
    "epollCreate" (fmap unEPollFd c_epoll_create)

epollControl :: EPollFd -> ControlOp -> CInt -> Ptr Event -> IO CInt
epollControl epfd op fd events =
    undefined

epollWait :: EPollFd -> Ptr Event -> Int -> Int -> IO Int
epollWait epfd events maxNumEvents timeout =
    undefined
