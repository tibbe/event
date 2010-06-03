{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

module System.Event.Select
    (
      new
    , available
    ) where

#include "EventConfig.h"

import Prelude hiding (exp)
import qualified System.Event.Internal as E

#if !defined(HAVE_SELECT)
new :: IO E.Backend
new = error "Select back end not implemented for this platform"

available :: Bool
available = False
{-# INLINE available #-}
#else

#if defined(HAVE_SYS_SELECT_H)
#include <sys/select.h>
#endif
#if defined(HAVE_SYS_TIME_H)
#include <sys/time.h>
#endif
#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif
#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef, writeIORef)
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)
import System.Event.Clock (CTimeval(..))
import System.Posix.Types (Fd(..))

------------------------------------------------------------------------
-- Exported interface

new :: IO E.Backend
new = do
    rdfds <- mallocForeignPtrBytes (#size fd_set)
    wrfds <- mallocForeignPtrBytes (#size fd_set)
    new' <- newIORef []
    pending' <- newIORef []
    return $ E.backend select modifyFd (const $ return ())
        (Select rdfds wrfds new' pending')

available :: Bool
available = True
{-# INLINE available #-}

select :: Select -> E.Timeout -> (Fd -> E.Event -> IO ()) -> IO ()
select be to f = do
    -- Pick up new I/O requests.
    new' <- atomicModifyIORef (newReqs be) $ \xs -> ([], xs)
    old' <- readIORef (pendingReqs be)
    let reqs = new' ++ old'

    -- Build fd_sets for select().
    reqs' <- withForeignPtr (readfds be) $ \rdp ->
        withForeignPtr (writefds be) $ \wrp ->
        withTimeout to $ \top -> do
            fdZero rdp
            fdZero wrp
            maxfd <- buildFdSets 0 rdp wrp reqs
            n <- E.throwErrnoIfMinus1NoRetry "c_select" $
                c_select (maxfd + 1) rdp wrp nullPtr top

            if n == 0 then return reqs
                else completeRequests f reqs rdp wrp []

    writeIORef (pendingReqs be) reqs'

withTimeout :: E.Timeout -> (Ptr CTimeval -> IO a) -> IO a
withTimeout E.Forever f     = f nullPtr
withTimeout (E.Timeout t) f =
    let !tval = CTimeval (floor t) (floor $ t * 1000000.0)
    in with tval f

buildFdSets :: Fd -> Ptr CFdSet -> Ptr CFdSet -> [IOReq] -> IO Fd
buildFdSets maxfd _ _ []      = return maxfd
buildFdSets maxfd rdfds wrfds (Read fd : reqs)
    | fd >= (#const FD_SETSIZE) =
        error "buildFdSets: file descriptor out of range"
    | otherwise = do
        fdSet fd rdfds
        buildFdSets (max maxfd fd) rdfds wrfds reqs
buildFdSets maxfd rdfds wrfds (Write fd : reqs)
    | fd >= (#const FD_SETSIZE) =
        error "buildFdSets: file descriptor out of range"
    | otherwise = do
        fdSet fd wrfds
        buildFdSets (max maxfd fd) rdfds wrfds reqs

completeRequests :: (Fd -> E.Event -> IO ()) -> [IOReq] -> Ptr CFdSet
                 -> Ptr CFdSet -> [IOReq] -> IO [IOReq]
completeRequests _ [] _ _ reqs' = return reqs'
completeRequests f (Read fd : reqs) rdfds wrfds reqs' = do
    b <- fdIsSet fd rdfds
    if b
        then f fd E.evtRead >> completeRequests f reqs rdfds wrfds reqs'
        else completeRequests f reqs rdfds wrfds (Read fd : reqs')
completeRequests f (Write fd : reqs) rdfds wrfds reqs' = do
    b <- fdIsSet fd wrfds
    if b
        then f fd E.evtWrite >> completeRequests f reqs rdfds wrfds reqs'
        else completeRequests f reqs rdfds wrfds (Write fd : reqs')

modifyFd :: Select -> Fd -> E.Event -> E.Event -> IO ()
modifyFd be fd _oevt nevt = do
    when (nevt `E.eventIs` E.evtRead) $ atomicModifyIORef (newReqs be) $ \xs ->
        ((Read fd : xs), ())
    when (nevt `E.eventIs` E.evtWrite) $ atomicModifyIORef (newReqs be) $ \xs ->
        ((Write fd : xs), ())

------------------------------------------------------------------------
-- FFI binding

data IOReq = Read {-# UNPACK #-} !Fd
           | Write {-# UNPACK #-} !Fd

data Select = Select {
      readfds     :: {-# UNPACK #-} !(ForeignPtr CFdSet)
    , writefds    :: {-# UNPACK #-} !(ForeignPtr CFdSet)
    , newReqs     :: {-# UNPACK #-} !(IORef [IOReq])
    , pendingReqs :: {-# UNPACK #-} !(IORef [IOReq])
    }

data CFdSet

fdIsSet :: Fd -> Ptr CFdSet -> IO Bool
fdIsSet (Fd fd) fdset = do
    b <- c_fdIsSet fd fdset
    if b /= 0
        then return True
        else return False

fdSet :: Fd -> Ptr CFdSet -> IO ()
fdSet (Fd fd) fdset = c_fdSet fd fdset

fdZero :: Ptr CFdSet -> IO ()
fdZero fdset = c_fdZero fdset

foreign import ccall unsafe "select"
    c_select :: Fd -> Ptr CFdSet -> Ptr CFdSet -> Ptr CFdSet -> Ptr CTimeval
             -> IO CInt

foreign import ccall unsafe "__hsevent_fd_isset"
    c_fdIsSet :: CInt -> Ptr CFdSet -> IO CInt

foreign import ccall unsafe "__hsevent_fd_set"
    c_fdSet :: CInt -> Ptr CFdSet -> IO ()

foreign import ccall unsafe "__hsevent_fd_zero"
    c_fdZero :: Ptr CFdSet -> IO ()

#endif /* defined(HAVE_SELECT) */
