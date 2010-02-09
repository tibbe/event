module Manager (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad (replicateM_)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.Marshal (alloca)
import System.Event.Control (setNonBlockingFD)
import System.Event.Internal (Backend)
import System.Event.Manager
import System.Posix.IO (createPipe)
import System.Posix.Internals (c_read, c_write)
import Test.HUnit (Assertion, assertBool, assertEqual)
import qualified System.Event.EPoll as EPoll
import qualified System.Event.KQueue as KQueue
import qualified System.Event.Poll as Poll
import qualified Test.Framework as F
import qualified Test.Framework.Providers.HUnit as F

withBackend :: IO Backend -> (EventManager -> IO a) -> IO a
withBackend what act = do
  mgr <- newWith =<< what
  forkIO $ loop mgr
  a <- act mgr `finally` shutdown mgr
  assertBool "finished" =<< finished mgr
  return a

-- Ensure that we can create and tear down many backends without
-- leaking limited resources such as file descriptors.
createN :: IO Backend -> Assertion
createN what = replicateM_ 10000 . withBackend what $ \_mgr -> return ()

-- Send and receive a single byte through a pipe.
pipe :: IO Backend -> Assertion
pipe what = withBackend what $ \mgr -> do
  (rd,wr) <- createPipe
  setNonBlockingFD (fromIntegral rd)
  setNonBlockingFD (fromIntegral wr)
  done <- newEmptyMVar
  let canRead fdk evt = do
        assertEqual "read fd" (keyFd fdk) rd
        assertEqual "read event" evt evtRead
        alloca $ \p ->
          throwErrnoIfMinus1_ "read" $
            c_read (fromIntegral (keyFd fdk)) p 1
        putMVar done ()
      canWrite fdk evt = do
        assertEqual "write fd" (keyFd fdk) wr
        assertEqual "write event" evt evtWrite
        alloca $ \p ->
          throwErrnoIfMinus1_ "write" $
            c_write (fromIntegral (keyFd fdk)) p 1
  registerFd mgr canRead rd evtRead
  registerFd mgr canWrite wr evtWrite
  takeMVar done

backendTests :: IO Backend -> [F.Test]
backendTests what = map ($what) [
   F.testCase "createN" . createN
 , F.testCase "pipe" . pipe
 ]

tests :: F.Test
tests = F.testGroup "System.Event.Manager" [ group | (available, group) <- [ 
          (EPoll.available,  F.testGroup "EPoll"  $ backendTests EPoll.new)
        , (KQueue.available, F.testGroup "KQueue" $ backendTests KQueue.new)
        , (Poll.available,   F.testGroup "Poll"   $ backendTests Poll.new)
        ], available]
