{-# LANGUAGE BangPatterns #-}

module System.Event.TimeoutTable.Internal where

import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe (fromJust)
import           Prelude hiding (null)


------------------------------------------------------------------------------
{-|

A TimeoutTable is a key-value mapping with an associated timeout value. You
can:

  * look up, query, delete, or modify values by key

  * find the oldest (i.e. first to expire) entry in the table

  * update the timeout value for a key

-}

data TimeoutTable tm k a = TimeoutTable
    { _keySet  :: !(Map k (tm, a))
    , _timeSet :: !(Map tm [k]) }


------------------------------------------------------------------------------
instance (Show tm, Show k, Show a) => Show (TimeoutTable tm k a) where
    show (TimeoutTable ks _) = "<TimeoutTable (" ++ show (fmap f ks) ++ ")>"

      where
        f :: (Show tm, Show a) => (tm, a) -> String
        f x = show x



------------------------------------------------------------------------------
-- internal functions follow
------------------------------------------------------------------------------

removeFromTimeSet :: (Ord k, Ord tm) =>
                     tm
                  -> k
                  -> Map tm [k]
                  -> Map tm [k]
removeFromTimeSet tm k ts = killIt old
  where
    old       = fromJust $ Map.lookup tm ts
    killIt ks = if List.null ks'
                  then Map.delete tm ts
                  else Map.insert tm ks' ts
      where
        !ks' = List.delete k ks
