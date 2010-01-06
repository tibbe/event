{-# LANGUAGE ForeignFunctionInterface #-}

module System.Event.KQueue where

import Control.Monad
import Data.Bits
import Data.Monoid (Monoid(..))
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (filter)
import System.Posix.Types (Fd(..))

import qualified System.Event.Internal as E
import           System.Event.Internal (Timeout(..))
import qualified System.Event.Array as A

#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>

------------------------------------------------------------------------
-- FFI binding

newtype EventQ = EventQ { unEventQ :: CInt }
    deriving (Eq, Show)

data Event = Event {
      ident  :: {-# UNPACK #-} !CUIntPtr
    , filter :: {-# UNPACK #-} !Filter
    , flags  :: {-# UNPACK #-} !Flag
    , fflags :: {-# UNPACK #-} !CUInt
    , data_  :: {-# UNPACK #-} !CIntPtr
    , udata  :: {-# UNPACK #-} !(Ptr ())
    } deriving Show

instance Storable Event where
    sizeOf _ = #size struct kevent
    alignment _ = alignment (undefined :: CInt)

    peek ptr = do
        ident'  <- #{peek struct kevent, ident} ptr
        filter' <- #{peek struct kevent, filter} ptr
        flags'  <- #{peek struct kevent, flags} ptr
        fflags' <- #{peek struct kevent, fflags} ptr
        data'   <- #{peek struct kevent, data} ptr
        udata'  <- #{peek struct kevent, udata} ptr
        return $ Event ident' (Filter filter') (Flag flags') fflags' data'
            udata'

    poke ptr ev = do
        #{poke struct kevent, ident} ptr (ident ev)
        #{poke struct kevent, filter} ptr (unFilter $ filter ev)
        #{poke struct kevent, flags} ptr (unFlag $ flags ev)
        #{poke struct kevent, fflags} ptr (fflags ev)
        #{poke struct kevent, data} ptr (data_ ev)
        #{poke struct kevent, udata} ptr (udata ev)

newtype Flag = Flag { unFlag :: CUShort }
    deriving (Eq, Show)

#{enum Flag, Flag
 , flagAdd     = EV_ADD
 , flagEnable  = EV_ENABLE
 , flagDisable = EV_DISABLE
 , flagDelete  = EV_DELETE
 , flagReceipt = EV_RECEIPT
 , flagOneShot = EV_ONESHOT
 , flagClear   = EV_CLEAR
 , flagEof     = EV_EOF
 , flagError   = EV_ERROR
 }

combineFlags :: [Flag] -> Flag
combineFlags = Flag . foldr ((.|.) . unFlag) 0

newtype Filter = Filter { unFilter :: CShort }
    deriving (Eq, Show)

#{enum Filter, Filter
 , filterRead   = EVFILT_READ
 , filterWrite  = EVFILT_WRITE
 , filterAio    = EVFILT_AIO
 , filterVnode  = EVFILT_VNODE
 , filterProc   = EVFILT_PROC
 , filterSignal = EVFILT_SIGNAL
 , filterTimer  = EVFILT_TIMER
 }

combineFilters :: [Filter] -> Filter
combineFilters = Filter . foldr ((.|.) . unFilter) 0

data TimeSpec = TimeSpec {
      tv_sec  :: {-# UNPACK #-} !CTime
    , tv_nsec :: {-# UNPACK #-} !CLong
    }

instance Storable TimeSpec where
    sizeOf _ = #size struct timespec
    alignment _ = alignment (undefined :: CInt)

    peek ptr = do
        tv_sec'  <- #{peek struct timespec, tv_sec} ptr
        tv_nsec' <- #{peek struct timespec, tv_nsec} ptr
        return $ TimeSpec tv_sec' tv_nsec'

    poke ptr ts = do
        #{poke struct timespec, tv_sec} ptr (tv_sec ts)
        #{poke struct timespec, tv_nsec} ptr (tv_nsec ts)

foreign import ccall unsafe "event.h kqueue"
    c_kqueue :: IO EventQ

foreign import ccall unsafe "event.h kevent"
    c_kevent :: EventQ -> Ptr Event -> CInt -> Ptr Event -> CInt
             -> Ptr TimeSpec -> IO CInt

kqueue :: IO EventQ
kqueue = EventQ `fmap` throwErrnoIfMinus1
         "kqueue" (fmap unEventQ c_kqueue)

-- TODO: We cannot retry on EINTR as the timeout would be wrong.
-- Perhaps we should just return without calling any callbacks.
kevent :: EventQ -> Ptr Event -> Int -> Ptr Event -> Int -> Ptr TimeSpec
       -> IO Int
kevent k chs chlen evs evlen ts
    = fmap fromIntegral $ throwErrnoIfMinus1 "kevent" $
      c_kevent k chs (fromIntegral chlen) evs (fromIntegral evlen) ts

withTimeSpec :: TimeSpec -> (Ptr TimeSpec -> IO a) -> IO a
withTimeSpec ts f =
    if tv_sec ts < 0 then
        f nullPtr
      else
        alloca $ \ptr -> poke ptr ts >> f ptr

msToTimeSpec :: Timeout -> TimeSpec
msToTimeSpec Forever = TimeSpec (-1) (-1)
msToTimeSpec (Timeout ms) = TimeSpec (toEnum sec) (toEnum nanosec)
  where
    sec :: Int
    sec     = fromEnum $ ms `div` 1000

    nanosec :: Int
    nanosec = (fromEnum ms - 1000*sec) * 1000000

------------------------------------------------------------------------
-- Exported interface

data EventQueue = EventQueue {
      kq       :: {-# UNPACK #-} !EventQ
    , changes  :: {-# UNPACK #-} !(A.Array Event)
    , events   :: {-# UNPACK #-} !(A.Array Event)
    , eqWakeup :: {-# UNPACK #-} !E.Wakeup
    }

instance E.Backend EventQueue where
    new          = new
    poll         = poll
    set q fd evs = set q fd (fromEvent evs) flagAdd
    wakeup       = wakeup

new :: IO EventQueue
new = do
    eq <- liftM4 EventQueue kqueue A.empty (A.new 64) E.createWakeup
    set eq (E.wakeupReadFd . eqWakeup $ eq) filterRead flagAdd
    return eq

set :: EventQueue -> Fd -> Filter -> Flag -> IO ()
set q fd fltr flg =
    A.snoc (changes q) (Event (fromIntegral fd) fltr flg 0 0 nullPtr)

poll :: EventQueue
     -> Timeout
     -> (Fd -> E.Event -> IO ())
     -> IO E.Result
poll q tout f = do
    changesLen <- A.length (changes q)
    len <- A.length (events q)
    when (changesLen > len) $ A.ensureCapacity (events q) (2 * changesLen)
    res <- A.useAsPtr (changes q) $ \changesPtr chLen ->
               A.useAsPtr (events q) $ \eventsPtr evLen ->
               withTimeSpec (msToTimeSpec tout) $ \tsPtr ->
               kevent (kq q) changesPtr chLen eventsPtr evLen tsPtr

    if res == 0 then
        return E.TimedOut
      else do
        eventsLen <- A.length (events q)
        when (res == eventsLen) $ do
            A.ensureCapacity (events q) (2 * eventsLen)

        A.mapM_ (events q) $ \e ->
            f (fromIntegral . ident $ e) (toEvent . filter $ e)

        return E.Activity

wakeup :: EventQueue -> E.WakeupMessage -> IO ()
wakeup ep = E.writeWakeupMessage (eqWakeup ep)

fromEvent :: E.Event -> Filter
fromEvent e = Filter (remap E.evtRead (#const EVFILT_READ) .|.
                      remap E.evtWrite (#const EVFILT_WRITE))
  where remap evt to
            | e `E.eventIs` evt = to
            | otherwise         = 0

toEvent :: Filter -> E.Event
toEvent (Filter f) = remap (#const EVFILT_READ)  E.evtRead `mappend`
                     remap (#const EVFILT_WRITE) E.evtWrite
  where remap evt to
            | f .&. evt /= 0 = to
            | otherwise      = mempty
