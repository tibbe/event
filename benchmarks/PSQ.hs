{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import System.Event.PSQ (PSQ)
import qualified System.Event.PSQ as Q

main = defaultMain
    [ bench "insert10k/min" $ whnf (Q.findMin . ascFrom) n
    , bench "delete1k/min" $ whnf (Q.findMin . deleteEveryN (n `div` 1000) n) q
    ]
  where
    -- Number of elements
    n = 10000

    -- Priority queue with 'n' elements
    q = ascFrom n

-- | Create a priority queue with keys and priorities in ascending
-- order starting at 0 and ending at @max@ (exclusive.)
ascFrom :: Int -> PSQ
ascFrom max = go 0 Q.empty
  where
    go :: Int -> PSQ -> PSQ
    go n !q
        | n >= max  = q
        | otherwise = go (n + 1) $ Q.insert n n q

-- | Delete all keys that are multiples of @step@ but less than @max@.
deleteEveryN :: Int -> Int -> PSQ -> PSQ
deleteEveryN step max q0 = go 0 q0
  where
    go :: Int -> PSQ -> PSQ
    go n !q
        | n >= max  = q
        | otherwise = go (n + step) $ Q.delete n q
