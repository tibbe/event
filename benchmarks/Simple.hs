-- Flow:
--
-- 1. Create N pipes.
--
-- Modelled after:
-- http://levent.svn.sourceforge.net/viewvc/levent/trunk/libevent/test/bench.c

module Main where

import Control.Monad
import Data.Array.IArray
import Data.Array.Unboxed
import System.Posix.IO
import System.Posix.Resource
import System.Posix.Types

numPipes :: Int
numPipes = 1024

main :: IO ()
main = do
    -- Increase the maximum number of file descriptors to fit the
    -- number of pipes.
    let lim = ResourceLimit $ (fromIntegral numPipes) * 2 + 50
    setResourceLimit ResourceOpenFiles
        (ResourceLimits { softLimit = lim, hardLimit = lim })

    -- Create the pipes.
    ps <- concatMap (\(Fd x, Fd y) -> [fromIntegral x, fromIntegral y]) `fmap`
          replicateM numPipes createPipe
    let pipes = listArray (0, numPipes) ps :: UArray Int Int
    return ()
