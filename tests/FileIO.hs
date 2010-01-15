import Control.Concurrent.MVar
import Control.Exception
import Data.IORef
import Foreign.C.Error (throwErrnoIfMinus1Retry_)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable
import System.Directory
import System.Event
import System.IO
import System.Posix.IO
import System.Posix.Internals (c_close, c_read, c_write)
import System.Posix.Types (Fd(..))

readCallback :: MVar () -> IORef Int -> Int -> Fd -> Event -> IO ()
readCallback done counter count (Fd fd) evt = do
  c <- atomicModifyIORef counter $ \x -> (x+1,x)
  if c == count
    then putMVar done ()
    else alloca $ \p -> do
      throwErrnoIfMinus1Retry_ "read" $ c_read fd p 1
      print =<< peek p

main = do
  let numBytes = 4
  bracket (openBinaryTempFile "." "FileIO.dat")
          (\(path, h) -> hClose h >> removeFile path) $ \(path,h) -> do
    hPutStr h (take numBytes ['a'..])
    hSeek h AbsoluteSeek 0
    mgr <- new
    done <- newEmptyMVar
    count <- newIORef 0
    fd <- handleToFd h
    registerFd mgr (readCallback done count numBytes) fd evtRead
    takeMVar done



