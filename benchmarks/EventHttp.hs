{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}

import Control.Concurrent
import Control.Exception (finally)
import Control.Monad
import Control.Monad.Error
import GHC.Conc hiding (ensureIOManagerIsRunning)
import System.Event as E
import System.Event.Thread
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import System.Posix.Types
import Network.Socket hiding (accept, recv)
import Network.Socket.Internal
import EventSocket (recv, sendAll)
import EventUtil (setNonBlocking)
import Data.ByteString.Char8 as B

main = do
  ensureIOManagerIsRunning
  let port = "5002"
      myHints = defaultHints { addrFlags = [AI_PASSIVE]
                             , addrSocketType = Stream }
  (ai:_) <- getAddrInfo (Just myHints) Nothing (Just port)
  sock <- socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai)
  setSocketOption sock ReuseAddr 1
  bindSocket sock (addrAddress ai)
  listen sock maxListenQueue
  mgrs <- replicateM numCapabilities E.new
  done <- newEmptyMVar
  let (MkSocket fd _ _ _ _) = sock
  forM_ mgrs $ \mgr -> do
    forkIO $ do
      E.init mgr
      registerFd_ mgr (\ _ _ -> accept mgr sock client) (fromIntegral fd) evtRead
      step mgr
      print "nah"
      putMVar done ()
  takeMVar done

repeatOnIntr :: IO (Either Errno a) -> IO (Either Errno a)
repeatOnIntr act = do
  ret <- act
  case ret of
    l@(Left err) -> if err == eINTR
                    then repeatOnIntr act
                    else return l
    r            -> return r

blocking :: EventManager
         -> IO (Either Errno a)
         -> (a -> IO ())
         -> IO ()
blocking mgr act on_success = do
  ret <- repeatOnIntr act
  case ret of
    Left err
        | err /= eWOULDBLOCK && err /= eAGAIN ->
            ioError (errnoToIOError "accept" err Nothing Nothing)
        | otherwise ->
            step mgr >> return ()
    Right a -> on_success a


accept :: EventManager -> Socket
       -> (EventManager -> Socket -> SockAddr -> IO ())
       -> IO ()
accept mgr sock@(MkSocket fd family stype proto _status) serve = do
  let sz = sizeOfSockAddrByFamily family
      act :: IO (Either Errno (CInt, SockAddr))
      act = allocaBytes sz $ \sockaddr -> do
            n <- with (fromIntegral sz) $ c_accept (fromIntegral fd) sockaddr
            if n == -1
              then Left `fmap` getErrno
              else do
                sa <- peekSockAddr sockaddr
                return $! Right (n, sa)
  blocking mgr act $ \(nfd,addr) -> do
    setNonBlocking (fromIntegral nfd)
    nsock <- MkSocket nfd family stype proto `fmap` newMVar Connected
    forkIO $ serve mgr nsock addr
    accept mgr sock serve
            
client :: EventManager -> Socket -> SockAddr -> IO ()
client _mgr sock _addr = (`finally` sClose sock) $ do
  recvRequest ""
  sendAll sock msg
 where
  msg = "HTTP/1.0 200 OK\r\nConnection: Close\r\nContent-Length: 5\r\n\r\nPong!"
  recvRequest r = do
    s <- recv sock 4096
    let t = B.append r s
    if B.null s || "\r\n\r\n" `B.isInfixOf` t
      then return ()
      else recvRequest t

foreign import ccall unsafe "sys/socket.h accept"
    c_accept :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> IO CInt
