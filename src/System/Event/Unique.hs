{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
module System.Event.Unique
    (
      UniqueSource
    , Unique(..)
    , newSource
    , newUnique
    ) where

import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.Int (Int64)

newtype UniqueSource = US (IORef Int64)

newtype Unique = Unique { asInt64 :: Int64 }
    deriving (Eq, Ord, Num)

instance Show Unique where
    show = show . asInt64

newSource :: IO UniqueSource
newSource = US `fmap` newIORef 0

newUnique :: UniqueSource -> IO Unique
newUnique (US ref) = do
    !v <- atomicModifyIORef ref $ \u -> let !u' = u+1 in (u', Unique u)
    return v -- be careful with modify functions!
