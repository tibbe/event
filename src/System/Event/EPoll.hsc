{-# LANGUAGE ForeignFunctionInterface #-}

module System.Event.EPoll where

#include <sys/epoll.h>

import Control.Monad (liftM3, when)
import Data.Bits ((.|.), (.&.))
import Data.Monoid (Monoid(..))
import Data.Word (Word32)
import Foreign.C.Error (throwErrnoIfMinus1, throwErrnoIfMinus1_)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import System.Posix.Types (Fd(..))

import qualified System.Event.Array    as A
import qualified System.Event.Internal as E
import           System.Event.Internal (Timeout(..))

data EPoll = EPoll {
      epollFd     :: !EPollFd
    , epollEvents :: !(A.Array Event)
    , epollWakeup :: !E.Wakeup
    }

instance E.Backend EPoll where
    new    = new
    set    = set
    poll   = poll
    wakeup = undefined

new :: IO EPoll
new = do
  ep <- liftM3 EPoll epollCreate (A.new 64) E.createWakeup
  set ep (E.wakeupReadFd . epollWakeup $ ep) E.evtRead
  return ep

set :: EPoll -> Fd -> E.Event -> IO ()
set ep fd events = with e $ epollControl (epollFd ep) controlOpAdd fd
  where e = Event (fromEvent events) fd

poll :: EPoll                        -- ^ state
     -> Timeout                      -- ^ timeout in milliseconds
     -> (Fd -> E.Event -> IO ()) -- ^ I/O callback
     -> IO E.Result
poll ep timeout f = do
    let epfd   = epollFd   ep
    let events = epollEvents ep

    n <- A.unsafeLoad events $ \es cap ->
         epollWait epfd es cap $ fromTimeout timeout

    if n == 0 then
        return E.TimedOut
      else do
        cap <- A.capacity events
        when (n == cap) $ A.ensureCapacity events (2 * cap)

        A.mapM_ events $ \e -> f (eventFd e) (toEvent (eventTypes e))

        return E.Activity

wakeup :: EPoll -> E.WakeupMessage -> IO ()
wakeup ep = E.writeWakeupMessage (epollWakeup ep)

newtype EPollFd = EPollFd CInt

data Event = Event {
      eventTypes :: EventType
    , eventFd    :: Fd
    } deriving (Show)

instance Storable Event where
    sizeOf    _ = #size struct epoll_event
    alignment _ = alignment (undefined :: CInt)

    peek ptr = do
        ets <- #{peek struct epoll_event, events} ptr
        ed  <- #{peek struct epoll_event, data.fd}   ptr
        return $ Event (EventType ets) ed

    poke ptr e = do
        #{poke struct epoll_event, events} ptr (unEventType $ eventTypes e)
        #{poke struct epoll_event, data.fd}   ptr (eventFd e)

newtype ControlOp = ControlOp CInt

#{enum ControlOp, ControlOp
 , controlOpAdd    = EPOLL_CTL_ADD
 , controlOpModify = EPOLL_CTL_MOD
 , controlOpDelete = EPOLL_CTL_DEL
 }

newtype EventType = EventType {
      unEventType :: Word32
    } deriving (Show)

epollCreate :: IO EPollFd
epollCreate =
    fmap EPollFd .
    throwErrnoIfMinus1 "epollCreate" $
    c_epoll_create 256 -- argument is ignored

epollControl :: EPollFd -> ControlOp -> Fd -> Ptr Event -> IO ()
epollControl (EPollFd epfd) (ControlOp op) (Fd fd) event =
    throwErrnoIfMinus1_ "epollControl" $ c_epoll_ctl epfd op fd event

epollWait :: EPollFd -> Ptr Event -> Int -> Int -> IO Int
epollWait (EPollFd epfd) events numEvents timeout =
    fmap fromIntegral .
    throwErrnoIfMinus1 "epollWait" $
    c_epoll_wait epfd events (fromIntegral numEvents) (fromIntegral timeout)

fromEvent :: E.Event -> EventType
fromEvent e = EventType (remap E.evtRead (#const EPOLLIN) .|.
                         remap E.evtWrite (#const EPOLLOUT))
  where remap evt to
            | e `E.eventIs` evt = to
            | otherwise         = 0

toEvent :: EventType -> E.Event
toEvent (EventType e) = remap (#const EPOLLIN) E.evtRead `mappend`
                        remap (#const EPOLLOUT) E.evtWrite
  where remap evt to
            | e .&. evt /= 0 = to
            | otherwise      = mempty

fromTimeout :: Timeout -> Int
fromTimeout Forever      = -1
fromTimeout (Timeout ms) = fromIntegral ms

foreign import ccall unsafe "sys/epoll.h epoll_create"
    c_epoll_create :: CInt -> IO CInt

foreign import ccall unsafe "sys/epoll.h epoll_ctl"
    c_epoll_ctl :: CInt -> CInt -> CInt -> Ptr Event -> IO CInt

foreign import ccall unsafe "sys/epoll.h epoll_wait"
    c_epoll_wait :: CInt -> Ptr Event -> CInt -> CInt -> IO CInt
