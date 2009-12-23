{-# LANGUAGE ForeignFunctionInterface #-}

module System.Event.EPoll where

#include <sys/epoll.h>

import Control.Monad (liftM, liftM2, when)
import Data.Bits ((.|.))
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt, CUInt)
import Foreign.Marshal.Error (void)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import System.Posix.Types (Fd(..))

import qualified System.Event.Array    as A
import qualified System.Event.Internal as E
import           System.Event.Internal (Timeout(..))

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

newtype EPollFd = EPollFd
    { unEPollFd :: Fd
    }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Event = Event
    { eventTypes :: EventType
    , eventFd    :: Fd
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

foreign import ccall unsafe "sys/epoll.h epoll_create"
    c_epoll_create :: CInt -> IO CInt

foreign import ccall unsafe "sys/epoll.h epoll_ctl"
    c_epoll_ctl :: CInt -> CInt -> CInt -> Ptr Event -> IO CInt

foreign import ccall unsafe "sys/epoll.h epoll_wait"
    c_epoll_wait :: CInt -> Ptr Event -> CInt -> CInt -> IO CInt

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

epollCreate :: IO EPollFd
epollCreate =
    EPollFd `fmap` throwErrnoIfMinus1 "epollCreate" (liftM Fd $ c_epoll_create size)
  where
    -- From manpage EPOLL_CREATE(2): "Since Linux 2.6.8, the size argument is
    -- unused. (The kernel dynamically sizes the required data structures
    -- without needing this initial hint.)" We pass 256 because libev does.
    size = 256 :: CInt

epollControl :: EPollFd -> ControlOp -> CInt -> Ptr Event -> IO ()
epollControl epfd op fd event =
    void $
      throwErrnoIfMinus1
        "epollControl"
        (c_epoll_ctl
           (fromIntegral $ unEPollFd epfd)
           (unControlOp op)
           (fromIntegral fd)
           event)

epollWait :: EPollFd -> Ptr Event -> Int -> Int -> IO Int
epollWait epfd events maxNumEvents maxNumMilliseconds =
    fmap fromIntegral $
      throwErrnoIfMinus1
        "epollWait"
        (c_epoll_wait
           (fromIntegral $ unEPollFd epfd)
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
    new    = new
    set    = set
    poll   = poll
    wakeup = wakeup

new :: IO EPoll
new = liftM2 EPoll epollCreate (A.new 64)

set :: EPoll -> Fd -> [E.Event] -> IO ()
set ep fd events =
    with e $ epollControl (epollEpfd ep) controlOpAdd (fromIntegral fd)
  where
    e   = Event ets fd
    ets = combineEventTypes (map fromEvent events)

poll :: EPoll                        -- ^ state
     -> Timeout                      -- ^ timeout in milliseconds
     -> (Fd -> [E.Event] -> IO ()) -- ^ I/O callback
     -> IO E.Result
poll ep timeout f = do
    let epfd   = epollEpfd   ep
    let events = epollEvents ep

    n <- A.unsafeLoad events $ \es cap ->
         epollWait epfd es cap $ fromTimeout timeout

    if n == 0 then
        return E.TimedOut
      else do
        cap <- A.capacity events
        when (n == cap) $ A.ensureCapacity events (2 * cap)

        A.mapM_ events $ \e -> f (eventFd e) []

        return E.Activity

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

fromEvent :: E.Event -> EventType
fromEvent E.Read  = eventTypeReadyForRead
fromEvent E.Write = eventTypeReadyForWrite


fromTimeout :: Timeout -> Int
fromTimeout Forever      = -1
fromTimeout (Timeout ms) = fromIntegral ms
