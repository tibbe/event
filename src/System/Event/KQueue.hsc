{-# LANGUAGE ForeignFunctionInterface, RecordWildCards #-}

module System.Event.KQueue where

import Control.Concurrent.MVar (MVar, newMVar, swapMVar, withMVar)
import Control.Monad
import Data.Bits
import Data.Int
import Data.Word
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
#include "EventConfig.h"

------------------------------------------------------------------------
-- Exported interface

data EventQueue = EventQueue {
      eqFd       :: {-# UNPACK #-} !QueueFd
    , eqChanges  :: {-# UNPACK #-} !(MVar (A.Array Event))
    , eqEvents   :: {-# UNPACK #-} !(A.Array Event)
    }

instance E.Backend EventQueue where
    new  = new
    poll = poll
    set  = set

new :: IO EventQueue
new = liftM3 EventQueue kqueue (newMVar =<< A.empty) (A.new 64)

set :: EventQueue -> Fd -> E.Event -> IO ()
set q fd evt = withMVar (eqChanges q) $ \ch ->
    case undefined of
      _ | evt `E.eventIs` E.evtRead  -> addChange ch filterRead
        | evt `E.eventIs` E.evtWrite -> addChange ch filterWrite
        | otherwise                  -> error $ "set: bad event " ++ show evt
  where addChange ch filt = A.snoc ch $ event fd filt flagAdd

poll :: EventQueue
     -> Timeout
     -> (Fd -> E.Event -> IO ())
     -> IO E.Result
poll EventQueue{..} tout f = do
    changes <- swapMVar eqChanges =<< A.empty
    changesLen <- A.length changes
    len <- A.length eqEvents
    when (changesLen > len) $ A.ensureCapacity eqEvents (2 * changesLen)
    n <- A.useAsPtr changes $ \changesPtr chLen ->
           A.unsafeLoad eqEvents $ \evPtr evCap ->
             withTimeSpec (msToTimeSpec tout) $
               kevent eqFd changesPtr chLen evPtr evCap

    if n == 0 then
        return E.TimedOut
      else do
        cap <- A.capacity eqEvents
        when (n == cap) $
          A.ensureCapacity eqEvents (2 * cap)

        A.forM_ eqEvents $ \e -> f (fromIntegral (ident e)) (toEvent (filter e))

        return E.Activity

------------------------------------------------------------------------
-- FFI binding

newtype QueueFd = QueueFd CInt
    deriving (Eq, Show)

#if defined(HAVE_KEVENT64)
data Event = KEvent64 {
      ident  :: {-# UNPACK #-} !Word64
    , filter :: {-# UNPACK #-} !Filter
    , flags  :: {-# UNPACK #-} !Flag
    , fflags :: {-# UNPACK #-} !Word32
    , data_  :: {-# UNPACK #-} !Int64
    , udata  :: {-# UNPACK #-} !Word64
    , ext0   :: {-# UNPACK #-} !Word64
    , ext1   :: {-# UNPACK #-} !Word64
    } deriving Show

event :: Fd -> Filter -> Flag -> Event
event fd filt flag = KEvent64 (fromIntegral fd) filt flag 0 0 0 0 0

instance Storable Event where
    sizeOf _ = #size struct kevent64_s
    alignment _ = alignment (undefined :: CInt)

    peek ptr = do
        ident'  <- #{peek struct kevent64_s, ident} ptr
        filter' <- #{peek struct kevent64_s, filter} ptr
        flags'  <- #{peek struct kevent64_s, flags} ptr
        fflags' <- #{peek struct kevent64_s, fflags} ptr
        data'   <- #{peek struct kevent64_s, data} ptr
        udata'  <- #{peek struct kevent64_s, udata} ptr
        ext0'   <- #{peek struct kevent64_s, ext[0]} ptr
        ext1'   <- #{peek struct kevent64_s, ext[1]} ptr
        return $! KEvent64 ident' (Filter filter') (Flag flags') fflags' data'
                           udata' ext0' ext1'

    poke ptr ev = do
        #{poke struct kevent64_s, ident} ptr (ident ev)
        #{poke struct kevent64_s, filter} ptr (unFilter $ filter ev)
        #{poke struct kevent64_s, flags} ptr (unFlag $ flags ev)
        #{poke struct kevent64_s, fflags} ptr (fflags ev)
        #{poke struct kevent64_s, data} ptr (data_ ev)
        #{poke struct kevent64_s, udata} ptr (udata ev)
        #{poke struct kevent64_s, ext[0]} ptr (ext0 ev)
        #{poke struct kevent64_s, ext[1]} ptr (ext1 ev)
#else
data Event = KEvent {
      ident  :: {-# UNPACK #-} !CUIntPtr
    , filter :: {-# UNPACK #-} !Filter
    , flags  :: {-# UNPACK #-} !Flag
    , fflags :: {-# UNPACK #-} !Word32
    , data_  :: {-# UNPACK #-} !CIntPtr
    , udata  :: {-# UNPACK #-} !(Ptr ())
    } deriving Show

event :: Fd -> Filter -> Flag -> Event
event fd filt flag = KEvent (fromIntegral fd) filt flag 0 0 nullPtr

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
        return $! KEvent ident' (Filter filter') (Flag flags') fflags' data'
                        udata'

    poke ptr ev = do
        #{poke struct kevent, ident} ptr (ident ev)
        #{poke struct kevent, filter} ptr (unFilter $ filter ev)
        #{poke struct kevent, flags} ptr (unFlag $ flags ev)
        #{poke struct kevent, fflags} ptr (fflags ev)
        #{poke struct kevent, data} ptr (data_ ev)
        #{poke struct kevent, udata} ptr (udata ev)
#endif

newtype Flag = Flag { unFlag :: Word16 }
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

kqueue :: IO QueueFd
kqueue = QueueFd `fmap` throwErrnoIfMinus1 "kqueue" c_kqueue

-- TODO: We cannot retry on EINTR as the timeout would be wrong.
-- Perhaps we should just return without calling any callbacks.
kevent :: QueueFd -> Ptr Event -> Int -> Ptr Event -> Int -> Ptr TimeSpec
       -> IO Int
kevent k chs chlen evs evlen ts
    = fmap fromIntegral $ throwErrnoIfMinus1 "kevent" $
#if defined(HAVE_KEVENT64)
      c_kevent64 k chs (fromIntegral chlen) evs (fromIntegral evlen) 0 ts
#else
      c_kevent k chs (fromIntegral chlen) evs (fromIntegral evlen) ts
#endif

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

fromEvent :: E.Event -> Filter
fromEvent e = Filter (remap E.evtRead (#const EVFILT_READ) .|.
                      remap E.evtWrite (#const EVFILT_WRITE))
  where remap evt to
            | e `E.eventIs` evt = to
            | otherwise         = 0

toEvent :: Filter -> E.Event
toEvent (Filter f)
    | f == (#const EVFILT_READ) = E.evtRead
    | f == (#const EVFILT_WRITE) = E.evtWrite
    | otherwise = error $ "toEvent: unknonwn filter " ++ show f

foreign import ccall unsafe "sys/event.h kqueue"
    c_kqueue :: IO CInt

#if defined(HAVE_KEVENT64)
foreign import ccall safe "sys/event.h kevent64"
    c_kevent64 :: QueueFd -> Ptr Event -> CInt -> Ptr Event -> CInt -> CUInt
               -> Ptr TimeSpec -> IO CInt
#elif defined(HAVE_KEVENT)
foreign import ccall safe "sys/event.h kevent"
    c_kevent :: QueueFd -> Ptr Event -> CInt -> Ptr Event -> CInt
             -> Ptr TimeSpec -> IO CInt
#else
#error no kevent system call available
#endif
