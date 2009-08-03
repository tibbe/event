{-# LANGUAGE ForeignFunctionInterface #-}

module System.Event.EPoll where

#include <sys/epoll.h>

import Control.Monad (liftM2, when)
import Data.Bits ((.|.))
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt, CUInt)
import Foreign.Marshal.Error (void)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

import qualified System.Event.Array    as A
import qualified System.Event.Internal as E

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

newtype EPollFd = EPollFd
    { unEPollFd :: CInt
    }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Event = Event
    { eventTypes :: EventType
    , eventFd    :: CInt
    }

instance Storable Event where
    sizeOf    _ = #size struct epoll_event
    alignment _ = alignment (undefined :: CInt)

    peek ptr = do
        ets <- #{peek struct epoll_event, events} ptr
        ed  <- #{peek struct epoll_event, data}   ptr
        return $ Event (EventType ets) ed

    poke ptr e = do
        #{poke struct epoll_event, events} ptr (unEventType $ eventTypes e)
        #{poke struct epoll_event, data}   ptr (eventFd e)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

newtype ControlOp = ControlOp
    { unControlOp :: CInt
    }

#{enum ControlOp, ControlOp
 , controlOpAdd    = EPOLL_CTL_ADD
 , controlOpModify = EPOLL_CTL_MOD
 , controlOpDelete = EPOLL_CTL_DEL
 }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

newtype EventType = EventType
    { unEventType :: CUInt
    }

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

foreign import ccall unsafe "ep.h epoll_create"
    c_epoll_create :: CInt -> IO CInt

foreign import ccall unsafe "ep.h epoll_ctl"
    c_epoll_ctl :: CInt -> CInt -> CInt -> Ptr Event -> IO CInt

foreign import ccall unsafe "ep.h epoll_wait"
    c_epoll_wait :: CInt -> Ptr Event -> CInt -> CInt -> IO CInt

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

epollCreate :: IO EPollFd
epollCreate =
    EPollFd `fmap` throwErrnoIfMinus1 "epollCreate" (c_epoll_create size)
  where
    -- From manpage EPOLL_CREATE(2): "Since Linux 2.6.8, the size argument is
    -- unused. (The kernel dynamically sizes the required data structures without
    -- needing this initial hint.)" We pass 256 because libev does.
    size = 256 :: CInt

epollControl :: EPollFd -> ControlOp -> CInt -> Ptr Event -> IO ()
epollControl epfd op fd event =
    void $
      throwErrnoIfMinus1
        "epollControl"
        (c_epoll_ctl (unEPollFd epfd) (unControlOp op) fd event)

epollWait :: EPollFd -> Ptr Event -> Int -> Int -> IO Int
epollWait epfd events maxNumEvents maxNumMilliseconds =
    fmap fromIntegral $
      throwErrnoIfMinus1
        "epollWait"
        (c_epoll_wait
           (unEPollFd epfd)
           events
           (fromIntegral maxNumEvents)
           (fromIntegral maxNumMilliseconds)
        )

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data EPoll = EPoll
    { epollEpfd   :: !EPollFd
    , epollEvents :: !(A.Array Event)
    }

instance E.Backend EPoll where
    new  = new
    set  = set
    poll = poll

new :: IO EPoll
new = liftM2 EPoll epollCreate (A.new 64)

set :: EPoll -> CInt -> [E.Event] -> IO ()
set ep fd events =
    with e $ \ePtr ->
      epollControl (epollEpfd ep) controlOpAdd fd ePtr
  where
    e   = Event ets fd
    ets = combineEventTypes (map fromEvent events)

poll :: EPoll -> (CInt -> [E.Event] -> IO ()) -> IO ()
poll ep f = do
    let epfd   = epollEpfd   ep
    let events = epollEvents ep

    A.useAsPtr events $ \eventsPtr eventsLen -> do
        n <- epollWait epfd eventsPtr eventsLen maxNumMilliseconds
        when (n > 0) $ putStrLn "events!"
        when (n == eventsLen) $ A.ensureCapacity events (2 * eventsLen)
        A.mapM_ events $ \e -> f (eventFd e) []
  where
    maxNumMilliseconds = 1000

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

fromEvent :: E.Event -> EventType
fromEvent E.Read  = eventTypeReadyForRead
fromEvent E.Write = eventTypeReadyForWrite
