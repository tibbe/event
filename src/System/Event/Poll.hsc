{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module System.Event.Poll where

#include "EventConfig.h"

#if defined(HAVE_POLL_H)
#include <poll.h>

import Control.Concurrent.MVar (MVar, newMVar, swapMVar, withMVar)
import Control.Monad (liftM2)
import Data.Bits (Bits, (.|.), (.&.))
import Data.Monoid (Monoid(..))
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt, CShort, CULong)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import qualified System.Event.Array as A
import qualified System.Event.Internal as E
import System.Posix.Types (Fd(..))

data Poll = Poll {
      pollChanges :: !(MVar (A.Array PollFd))
    , pollFd      :: !(A.Array PollFd)
    }

instance E.Backend Poll where
    new  = new
    poll = poll
    set  = set

new :: IO Poll
new = liftM2 Poll (newMVar =<< A.empty) A.empty

-- TODO: Modify an existing pollfd if one already exists for the given fd.
set :: Poll -> Fd -> E.Event -> IO ()
set p fd evt = withMVar (pollChanges p) $ \pfd ->
                 A.snoc pfd $ PollFd fd (fromEvent evt) 0

poll :: Poll
     -> E.Timeout
     -> (Fd -> E.Event -> IO ())
     -> IO E.Result
poll p tout f = do
  let a = pollFd p
  A.concat a =<< swapMVar (pollChanges p) =<< A.empty
  n <- A.useAsPtr a $ \ptr len -> throwErrnoIfMinus1 "c_poll" $
         c_poll ptr (fromIntegral len) (fromIntegral (fromTimeout tout))
  if n == 0
    then return E.TimedOut
    else do
      A.loop a 0 $ \i e -> do
        let r = pfdRevents e
        if r /= 0
          then do f (pfdFd e) (toEvent r)
                  let i' = i + 1
                  return (i', i' == n)
          else return (i, True)
      return E.Activity

fromTimeout :: E.Timeout -> Int
fromTimeout E.Forever      = -1
fromTimeout (E.Timeout ms) = fromIntegral ms

data PollFd = PollFd {
      pfdFd      :: {-# UNPACK #-} !Fd
    , pfdEvents  :: {-# UNPACK #-} !Event
    , pfdRevents :: {-# UNPACK #-} !Event
    } deriving (Show)

newtype Event = Event CShort
    deriving (Eq, Show, Num, Storable, Bits)

#{enum Event, Event
 , pollIn    = POLLIN
 , pollPri   = POLLPRI
 , pollOut   = POLLOUT
#ifdef POLLRDHUP
 , pollRdHup = POLLRDHUP
#endif
 , pollErr   = POLLERR
 , pollHup   = POLLHUP
 , pollNval  = POLLNVAL
 }

fromEvent :: E.Event -> Event
fromEvent e = remap E.evtRead  pollIn .|.
              remap E.evtWrite pollOut
  where remap evt to
            | e `E.eventIs` evt = to
            | otherwise         = 0

toEvent :: Event -> E.Event
toEvent e = remap (pollIn .|. pollErr .|. pollHup)  E.evtRead `mappend`
            remap (pollOut .|. pollErr .|. pollHup) E.evtWrite
  where remap evt to
            | e .&. evt /= 0 = to
            | otherwise      = mempty

instance Storable PollFd where
    sizeOf _    = #size struct pollfd
    alignment _ = alignment (undefined :: CInt)

    peek ptr = do
      fd <- #{peek struct pollfd, fd} ptr
      events <- #{peek struct pollfd, events} ptr
      revents <- #{peek struct pollfd, revents} ptr
      return $ PollFd fd events revents

    poke ptr p = do
      #{poke struct pollfd, fd} ptr (pfdFd p)
      #{poke struct pollfd, events} ptr (pfdEvents p)
      #{poke struct pollfd, revents} ptr (pfdRevents p)

foreign import ccall safe "poll.h poll"
    c_poll :: Ptr PollFd -> CULong -> CInt -> IO CInt

#endif /* defined(HAVE_POLL_H) */
