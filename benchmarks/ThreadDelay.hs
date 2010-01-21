{-# LANGUAGE CPP #-}

-- Benchmark 'threadDelay' by forking N threads which sleep for a
-- number of milliseconds and wait for them all to finish.

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (when)
import Data.IORef (atomicModifyIORef, newIORef)
import System.Event.Thread (ensureIOManagerIsRunning)

#if 1
import System.Event.Thread (threadDelay)
#else
import Control.Concurrent (threadDelay)
#endif

main = do
    ensureIOManagerIsRunning
    done <- newEmptyMVar
    ref <- newIORef 0
    let numThreads = 20000
    let loop :: Int -> IO ()
        loop i = do
            when (i < numThreads) $ do
                forkIO $ do threadDelay 1
                            a <- atomicModifyIORef ref $ \a ->
                                let !b = a+1 in (b,b)
                            when (a == numThreads) $ putMVar done ()
                loop (i + 1)
    loop 0
    takeMVar done
