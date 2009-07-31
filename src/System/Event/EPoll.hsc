{-# LANGUAGE ForeignFunctionInterface #-}

module System.Event.EPoll where

import Data.Bits ((.|.))
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt, CUInt)
import Foreign.Ptr (Ptr)

#include <sys/epoll.h>

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

newtype EPollFd = EPollFd
    { unEPollFd :: CInt
    } deriving (Eq, Show)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Event = Event
    { eventTypes :: EventType
    , eventData  :: Ptr ()
    } deriving Show

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

newtype ControlOp = ControlOp
    { unControlOp :: CInt
    } deriving (Eq, Show)

#{enum ControlOp, ControlOp
 , controlOpAdd    = EPOLL_CTL_ADD
 , controlOpModify = EPOLL_CTL_MOD
 , controlOpDelete = EPOLL_CTL_DEL
 }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

newtype EventType = EventType
    { unEventType :: CUInt
    } deriving (Eq, Show)

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

combineEventTypes :: [EventType] -> EventType
combineEventTypes = EventType . foldr ((.|.) . unEventType) 0

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

foreign import ccall unsafe "epoll.h epoll_create"
    c_epoll_create :: CInt -> IO EPollFd

foreign import ccall unsafe "epoll.h epoll_ctl"
    c_epoll_ctl :: EPollFd -> CInt -> CInt -> Ptr Event -> IO CInt

foreign import ccall unsafe "epoll.h epoll_wait"
    c_epoll_wait :: EPollFd -> Ptr Event -> CInt -> CInt -> IO CInt

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

epollCreate :: IO EPollFd
epollCreate =
    EPollFd `fmap` throwErrnoIfMinus1
                     "epollCreate"
                     (unEPollFd `fmap` c_epoll_create size)
  where
    -- From manpage EPOLL_CREATE(2): "Since Linux 2.6.8, the size argument is
    -- unused. (The kernel dynamically sizes the required data structures without
    -- needing this initial hint.)" We pass 256 because libev does.
    size = 256 :: CInt

epollControl :: EPollFd -> ControlOp -> CInt -> Ptr Event -> IO CInt
epollControl epfd op fd events =
    throwErrnoIfMinus1
      "epollControl"
      (c_epoll_ctl epfd (unControlOp op) fd events)

epollWait :: EPollFd -> Ptr Event -> Int -> Int -> IO CInt
epollWait epfd events maxNumEvents maxNumMilliseconds =
    throwErrnoIfMinus1
      "epollWait"
      (c_epoll_wait
         epfd
         events
         (fromIntegral maxNumEvents)
         (fromIntegral maxNumMilliseconds)
      )
