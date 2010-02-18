{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
module System.Event.Unique
    (
      UniqueSource
    , Unique(..)
    , newSource
    , newUnique
    ) where

import Data.Int (Int64)
import Control.Concurrent.STM

newtype UniqueSource = US (TVar Int64)

newtype Unique = Unique { asInt64 :: Int64 }
    deriving (Eq, Ord, Num)

instance Show Unique where
    show = show . asInt64

newSource :: IO UniqueSource
newSource = US `fmap` newTVarIO 0

newUnique :: UniqueSource -> IO Unique
newUnique (US ref) = do
    atomically $ do 
      u <- readTVar ref
      let !u' = u+1
      writeTVar ref u'
      return (Unique u')
{-# INLINE newUnique #-}
