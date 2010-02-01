{-# LANGUAGE CPP, OverloadedStrings #-}

-- Requires the network-bytestring library.
--
-- Start server and run
--   httperf --server=localhost --port=5002 --uri=/ --num-conns=10000

import Control.Concurrent (forkIO)
import Data.ByteString.Char8 ()
import Network.Socket hiding (accept, recv)
#if 0
import EventSocket (accept, recv, sendAll)
#else
import Network.Socket (accept)
import Network.Socket.ByteString (recv, sendAll)
#endif
import System.Event.Thread (ensureIOManagerIsRunning)

main = do
  ensureIOManagerIsRunning
  let myHints = defaultHints { addrFlags = [AI_PASSIVE] }
  (ai:_) <- getAddrInfo (Just myHints) Nothing (Just "5002")
  sock <- socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai)
  setSocketOption sock ReuseAddr 1
  bindSocket sock (addrAddress ai)
  listen sock 1024
  acceptConnections sock

acceptConnections :: Socket -> IO ()
acceptConnections sock = loop
  where
    loop = do
        (c,_) <- accept sock
        forkIO $ client c
        loop

client :: Socket -> IO ()
client sock = do
    recv sock 4096
    sendAll sock msg
    sClose sock

msg = "HTTP/1.0 200 OK\r\n\r\nContent-Length: 5\r\n\r\nPong!\r\n"
