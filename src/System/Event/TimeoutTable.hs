{-# LANGUAGE BangPatterns #-}

module System.Event.TimeoutTable
  ( TimeoutTable
  , empty
  , null
  , findOldest
  , find
  , fromList
  , member
  , insert
  , delete
  , update
  ) where

import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Prelude hiding (null)

import           System.Event.TimeoutTable.Internal

------------------------------------------------------------------------------
{-| An empty TimeoutTable. -}
empty :: TimeoutTable tm k a
empty = TimeoutTable Map.empty Map.empty


{-| Returns True if the table is empty. -}
null :: TimeoutTable tm k a -> Bool
null (TimeoutTable k _) = Map.null k


{-| Create a TimeoutTable from a list -}
fromList :: (Ord tm, Ord k) => [(tm, k, a)] -> TimeoutTable tm k a
fromList = List.foldl' insOne empty
  where
    insOne !tab (!t,!a,!b) = insert t a b tab


{-| Find the entry in the table with the first (oldest) expiry time. -}
findOldest :: (Ord tm, Ord k) => TimeoutTable tm k a -> Maybe (tm, k, a)
findOldest (TimeoutTable keys times) | Map.null keys = Nothing
                                     | otherwise = Just (t, hd, snd el)
  where
    (t,l) = Map.findMin times
    hd    = head l
    el    = fromJust $ Map.lookup hd keys


{-| Lookup a value by key. -}
find :: (Ord tm, Ord k) => k -> TimeoutTable tm k a -> Maybe (tm, a)
find k tab = Map.lookup k $ _keySet tab


{-| Is the given key a member of the table? -}
member :: (Ord tm, Ord k) => k -> TimeoutTable tm k a -> Bool
member k tab = Map.member k $ _keySet tab


{-| Add a new key-value-timeout mapping to the table. -}
insert :: (Ord tm, Ord k) =>
          tm                    -- ^ timeout for this mapping
       -> k                     -- ^ key
       -> a                     -- ^ value
       -> TimeoutTable tm k a   -- ^ table
       -> TimeoutTable tm k a
insert !tm !k !v !tab = TimeoutTable ks' ts'
  where
     !tab'       = delete k tab
     !ks         = _keySet tab'
     !ts         = _timeSet tab'

     !ks'        = Map.insert k (tm,v) ks
     !ts'        = Map.insertWith' consHead tm [k] ts


{-| Delete a key-value mapping from the table. -}
delete :: (Ord tm, Ord k) => k -> TimeoutTable tm k a -> TimeoutTable tm k a
delete !k !tab = maybe tab killIt mbTm
  where
    !ks            = _keySet tab
    !ts            = _timeSet tab
    !mbTm          = Map.lookup k ks

    killIt (!tm,_) = TimeoutTable ks' ts'
      where
        !ks'  = Map.delete k ks
        !ts'  = removeFromTimeSet tm k ts


{-| Update the timeout value for a key in the table. -}
update :: (Ord tm, Ord k) =>
          k                     -- ^ key to update
       -> tm                    -- ^ new timeout value
       -> TimeoutTable tm k a   -- ^ table
       -> TimeoutTable tm k a
update !k !tm !tab = maybe tab updateIt mbTm
  where
    !ks   = _keySet tab
    !ts   = _timeSet tab
    !mbTm = Map.lookup k ks

    updateIt (!oldTm, !v) = TimeoutTable ks' ts''
      where
        !ks'  = Map.insert k (tm,v) ks
        !ts'  = removeFromTimeSet oldTm k ts
        !ts'' = Map.insertWith' consHead tm [k] ts'


------------------------------------------------------------------------------
-- private functions follow
------------------------------------------------------------------------------

{-| Take the head of the first list and cons it to the second. Used w/
    `Map.insertWith'` to insert values into the time set. -}
consHead :: [a] -> [a] -> [a]
consHead !xs !ys = x:ys
  where
    !x = head xs
