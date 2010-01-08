-- Copyright (c) 2008, Ralf Hinze
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
--     * Redistributions of source code must retain the above
--       copyright notice, this list of conditions and the following
--       disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials
--       provided with the distribution.
--
--     * The names of the contributors may not be used to endorse or
--       promote products derived from this software without specific
--       prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
-- FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
-- COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
-- OF THE POSSIBILITY OF SUCH DAMAGE.

-- A /priority search queue/ (henceforth /queue/) efficiently supports
-- the opperations of both a search tree and a priority queue.  An
-- 'Elem'ent is a product of a key and a priority. Elements can be
-- inserted, deleted, modified and queried in logarithmic time, and
-- the binding with the least priority can be retrieved in constant
-- time.  A queue can be built from a list of elements, sorted by
-- keys, in linear time.
--
-- This implementation is due to Ralf Hinze with some modifications by
-- Scott Dillard and Johan Tibell.
--
-- * Hinze, R., /A Simple Implementation Technique for Priority Search
-- Queues/, ICFP 2001, pp. 110-121
--
-- <http://citeseer.ist.psu.edu/hinze01simple.html>

module System.Event.PSQ
    (
    -- * Binding Type
    Elem(..)
    , Key
    , Prio

    -- * Priority Search Queue Type
    , PSQ

    -- * Query
    , size
    , null
    , lookup

    -- * Construction
    , empty
    , singleton

    -- * Insertion
    , insert

    -- * Delete/Update
    , delete
    , adjust

    -- * Conversion
    , toList
    , toAscList
    , toDescList
    , fromList

    -- * Min
    , findMin
    , deleteMin
    , minView
    ) where

import Prelude hiding (lookup, null, foldl, foldr)
import qualified Prelude as P

-- | @E k p@ binds the key @k@ with the priority @p@.
data Elem = E
    { key   :: {-# UNPACK #-} !Key
    , prio  :: {-# UNPACK #-} !Prio
    } deriving Show

------------------------------------------------------------------------

-- | A mapping from keys @k@ to priorites @p@.

type Prio = Int
type Key = Int

data PSQ = Void
         | Winner {-# UNPACK #-} !Elem
                  !LTree
                  {-# UNPACK #-} !Key  -- max key

instance Show PSQ where
    show = show . map show . toAscList

-- | /O(1)/ The number of bindings in a queue.
size :: PSQ -> Int
size Void            = 0
size (Winner _ lt _) = 1 + size' lt

-- | /O(1)/ True if the queue is empty.
null :: PSQ -> Bool
null Void           = True
null (Winner _ _ _) = False

-- | /O(log n)/ The priority of a given key, or Nothing if the key is
-- not bound.
lookup :: Key -> PSQ -> Maybe Prio
lookup k q = case tourView q of
    Null -> Nothing
    Single (E k' p)
        | k == k'   -> Just p
        | otherwise -> Nothing
    tl `Play` tr
        | k <= maxKey tl -> lookup k tl
        | otherwise      -> lookup k tr

------------------------------------------------------------------------
-- Construction

empty :: PSQ
empty = Void

-- | O(1) Build a queue with one binding.
singleton :: Key -> Prio -> PSQ
singleton k p = Winner (E k p) Start k

------------------------------------------------------------------------
-- Insertion

-- | /O(log n)/ Insert a binding into the queue.
insert :: Key -> Prio -> PSQ -> PSQ
insert k p q = case tourView q of
    Null -> singleton k p
    Single (E k' p') -> case compare k k' of
        LT -> singleton k  p  `play` singleton k' p'
        EQ -> singleton k  p
        GT -> singleton k' p' `play` singleton k  p
    tl `Play` tr
        | k <= maxKey tl -> insert k p tl `play` tr
        | otherwise      -> tl `play` insert k p tr

------------------------------------------------------------------------
-- Delete/Update

-- | /O(log n)/ Remove a binding from the queue.
delete :: Key -> PSQ -> PSQ
delete k q = case tourView q of
    Null -> empty
    Single (E k' p)
        | k == k'   -> empty
        | otherwise -> singleton k' p
    tl `Play` tr
        | k <= maxKey tl -> delete k tl `play` tr
        | otherwise      -> tl `play` delete k tr

-- | /O(log n)/ Adjust the priority of a key.
adjust :: (Prio -> Prio) -> Key -> PSQ -> PSQ
adjust f k q = case tourView q of
    Null -> empty
    Single (E k' p)
        | k == k'   -> singleton k' (f p)
        | otherwise -> singleton k' p
    tl `Play` tr
        | k <= maxKey tl -> adjust f k tl `unsafePlay` tr
        | otherwise      -> tl `unsafePlay` adjust f k tr

------------------------------------------------------------------------
-- Conversion

-- | /O(n log n)/ Build a queue from a list of bindings.
fromList :: [Elem] -> PSQ
fromList = P.foldr (\(E k p) q -> insert k p q) empty

-- | /O(n)/ Convert a queue to a list.
toList :: PSQ -> [Elem]
toList = toAscList

-- | /O(n)/ Convert a queue to a list in ascending order of keys.
toAscList :: PSQ -> [Elem]
toAscList q  = seqToList (toAscLists q)

toAscLists :: PSQ -> Sequ Elem
toAscLists q = case tourView q of
    Null         -> emptySequ
    Single e     -> singleSequ e
    tl `Play` tr -> toAscLists tl <> toAscLists tr

-- | /O(n)/ Convert a queue to a list in descending order of keys.
toDescList :: PSQ -> [ Elem ]
toDescList q = seqToList (toDescLists q)

toDescLists :: PSQ -> Sequ Elem
toDescLists q = case tourView q of
    Null         -> emptySequ
    Single e     -> singleSequ e
    tl `Play` tr -> toDescLists tr <> toDescLists tl

------------------------------------------------------------------------
-- Min

-- | /O(1)/ The binding with the lowest priority.
findMin :: PSQ -> Maybe Elem
findMin Void           = Nothing
findMin (Winner e _ _) = Just e

-- | /O(log n)/ Remove the binding with the lowest priority.
deleteMin :: PSQ -> PSQ
deleteMin Void           = Void
deleteMin (Winner _ t m) = secondBest t m

-- | /O(log n)/ Retrieve the binding with the least priority, and the
-- rest of the queue stripped of that binding.
minView :: PSQ -> Maybe (Elem, PSQ)
minView Void           = Nothing
minView (Winner e t m) = Just (e, secondBest t m)

secondBest :: LTree -> Key -> PSQ
secondBest Start _                   = Void
secondBest (LLoser _ e tl m tr) m' = Winner e tl m `play` secondBest tr m'
secondBest (RLoser _ e tl m tr) m' = secondBest tl m `play` Winner e tr m'

------------------------------------------------------------------------
-- Loser tree

type Size = Int

data LTree = Start
           | LLoser {-# UNPACK #-} !Size
                    {-# UNPACK #-} !Elem
                    !LTree
                    {-# UNPACK #-} !Key  -- split key
                    !LTree
           | RLoser {-# UNPACK #-} !Size
                    {-# UNPACK #-} !Elem
                    !LTree
                    {-# UNPACK #-} !Key  -- split key
                    !LTree

size' :: LTree -> Size
size' Start              = 0
size' (LLoser s _ _ _ _) = s
size' (RLoser s _ _ _ _) = s

left, right :: LTree -> LTree

left Start                = moduleError "left" "empty loser tree"
left (LLoser _ _ tl _ _ ) = tl
left (RLoser _ _ tl _ _ ) = tl

right Start                = moduleError "right" "empty loser tree"
right (LLoser _ _ _  _ tr) = tr
right (RLoser _ _ _  _ tr) = tr

maxKey :: PSQ -> Key
maxKey Void           = moduleError "maxKey" "empty queue"
maxKey (Winner _ _ m) = m

lloser, rloser :: Key -> Prio -> LTree -> Key -> LTree -> LTree
lloser k p tl m tr = LLoser (1 + size' tl + size' tr) (E k p) tl m tr
rloser k p tl m tr = RLoser (1 + size' tl + size' tr) (E k p) tl m tr

------------------------------------------------------------------------
-- Balancing

-- | Balance factor
omega :: Int
omega = 4

lbalance, rbalance :: Key -> Prio -> LTree -> Key -> LTree -> LTree

lbalance k p l m r
    | size' l + size' r < 2     = lloser        k p l m r
    | size' r > omega * size' l = lbalanceLeft  k p l m r
    | size' l > omega * size' r = lbalanceRight k p l m r
    | otherwise                 = lloser        k p l m r

rbalance k p l m r
    | size' l + size' r < 2     = rloser        k p l m r
    | size' r > omega * size' l = rbalanceLeft  k p l m r
    | size' l > omega * size' r = rbalanceRight k p l m r
    | otherwise                 = rloser        k p l m r

lbalanceLeft :: Key -> Prio -> LTree -> Key -> LTree -> LTree
lbalanceLeft  k p l m r
    | size' (left r) < size' (right r) = lsingleLeft  k p l m r
    | otherwise                        = ldoubleLeft  k p l m r

lbalanceRight :: Key -> Prio -> LTree -> Key -> LTree -> LTree
lbalanceRight k p l m r
    | size' (left l) > size' (right l) = lsingleRight k p l m r
    | otherwise                        = ldoubleRight k p l m r

rbalanceLeft :: Key -> Prio -> LTree -> Key -> LTree -> LTree
rbalanceLeft  k p l m r
    | size' (left r) < size' (right r) = rsingleLeft  k p l m r
    | otherwise                        = rdoubleLeft  k p l m r

rbalanceRight :: Key -> Prio -> LTree -> Key -> LTree -> LTree
rbalanceRight k p l m r
    | size' (left l) > size' (right l) = rsingleRight k p l m r
    | otherwise                        = rdoubleRight k p l m r

lsingleLeft :: Key -> Prio -> LTree -> Key -> LTree -> LTree
lsingleLeft k1 p1 t1 m1 (LLoser _ (E k2 p2) t2 m2 t3)
    | p1 <= p2  = lloser k1 p1 (rloser k2 p2 t1 m1 t2) m2 t3
    | otherwise = lloser k2 p2 (lloser k1 p1 t1 m1 t2) m2 t3
lsingleLeft k1 p1 t1 m1 (RLoser _ (E k2 p2) t2 m2 t3) =
    rloser k2 p2 (lloser k1 p1 t1 m1 t2) m2 t3

rsingleLeft :: Key -> Prio -> LTree -> Key -> LTree -> LTree
rsingleLeft k1 p1 t1 m1 (LLoser _ (E k2 p2) t2 m2 t3) =
    rloser k1 p1 (rloser k2 p2 t1 m1 t2) m2 t3
rsingleLeft k1 p1 t1 m1 (RLoser _ (E k2 p2) t2 m2 t3) =
    rloser k2 p2 (rloser k1 p1 t1 m1 t2) m2 t3

lsingleRight :: Key -> Prio -> LTree -> Key -> LTree -> LTree
lsingleRight k1 p1 (LLoser _ (E k2 p2) t1 m1 t2) m2 t3 =
    lloser k2 p2 t1 m1 (lloser k1 p1 t2 m2 t3)
lsingleRight k1 p1 (RLoser _ (E k2 p2) t1 m1 t2) m2 t3 =
    lloser k1 p1 t1 m1 (lloser k2 p2 t2 m2 t3)

rsingleRight :: Key -> Prio -> LTree -> Key -> LTree -> LTree
rsingleRight k1 p1 (LLoser _ (E k2 p2) t1 m1 t2) m2 t3 =
    lloser k2 p2 t1 m1 (rloser k1 p1 t2 m2 t3)
rsingleRight k1 p1 (RLoser _ (E k2 p2) t1 m1 t2) m2 t3
    | p1 <= p2  = rloser k1 p1 t1 m1 (lloser k2 p2 t2 m2 t3)
    | otherwise = rloser k2 p2 t1 m1 (rloser k1 p1 t2 m2 t3)

ldoubleLeft :: Key -> Prio -> LTree -> Key -> LTree -> LTree
ldoubleLeft k1 p1 t1 m1 (LLoser _ (E k2 p2) t2 m2 t3) =
    lsingleLeft k1 p1 t1 m1 (lsingleRight k2 p2 t2 m2 t3)
ldoubleLeft k1 p1 t1 m1 (RLoser _ (E k2 p2) t2 m2 t3) =
    lsingleLeft k1 p1 t1 m1 (rsingleRight k2 p2 t2 m2 t3)

ldoubleRight :: Key -> Prio -> LTree -> Key -> LTree -> LTree
ldoubleRight k1 p1 (LLoser _ (E k2 p2) t1 m1 t2) m2 t3 =
    lsingleRight k1 p1 (lsingleLeft k2 p2 t1 m1 t2) m2 t3
ldoubleRight k1 p1 (RLoser _ (E k2 p2) t1 m1 t2) m2 t3 =
    lsingleRight k1 p1 (rsingleLeft k2 p2 t1 m1 t2) m2 t3

rdoubleLeft :: Key -> Prio -> LTree -> Key -> LTree -> LTree
rdoubleLeft k1 p1 t1 m1 (LLoser _ (E k2 p2) t2 m2 t3) =
    rsingleLeft k1 p1 t1 m1 (lsingleRight k2 p2 t2 m2 t3)
rdoubleLeft k1 p1 t1 m1 (RLoser _ (E k2 p2) t2 m2 t3) =
    rsingleLeft k1 p1 t1 m1 (rsingleRight k2 p2 t2 m2 t3)

rdoubleRight :: Key -> Prio -> LTree -> Key -> LTree -> LTree
rdoubleRight k1 p1 (LLoser _ (E k2 p2) t1 m1 t2) m2 t3 =
    rsingleRight k1 p1 (lsingleLeft k2 p2 t1 m1 t2) m2 t3
rdoubleRight k1 p1 (RLoser _ (E k2 p2) t1 m1 t2) m2 t3 =
    rsingleRight k1 p1 (rsingleLeft k2 p2 t1 m1 t2) m2 t3

play :: PSQ -> PSQ -> PSQ
Void `play` t' = t'
t `play` Void  = t
Winner e@(E k p) t m `play` Winner e'@(E k' p') t' m'
    | p <= p'   = Winner e (rbalance k' p' t m t') m'
    | otherwise = Winner e' (lbalance k p t m t') m'

unsafePlay :: PSQ -> PSQ -> PSQ
Void `unsafePlay` t' =  t'
t `unsafePlay` Void  =  t
Winner e@(E k p) t m `unsafePlay` Winner e'@(E k' p') t' m'
    | p <= p'   = Winner e (rbalance k' p' t m t') m'
    | otherwise = Winner e' (lbalance k p t m t') m'

data TourView = Null
              | Single {-# UNPACK #-} !Elem
              | PSQ `Play` PSQ

tourView :: PSQ -> TourView
tourView Void                 = Null
tourView (Winner e Start _) = Single e
tourView (Winner e (RLoser _ e' tl m tr) m') =
    Winner e tl m `Play` Winner e' tr m'
tourView (Winner e (LLoser _ e' tl m tr) m') =
    Winner e' tl m `Play` Winner e tr m'

------------------------------------------------------------------------
-- Utility functions

moduleError :: String -> String -> a
moduleError fun msg = error ("System.Event.PSQ." ++ fun ++ ':' : ' ' : msg)
{-# NOINLINE moduleError #-}

------------------------------------------------------------------------
-- Hughes's efficient sequence type

newtype Sequ a = Sequ ([a] -> [a])

emptySequ :: Sequ a
emptySequ = Sequ (\as -> as)

singleSequ :: a -> Sequ a
singleSequ a = Sequ (\as -> a : as)

(<>) :: Sequ a -> Sequ a -> Sequ a
Sequ x1 <> Sequ x2 = Sequ (\as -> x1 (x2 as))
infixr 5 <>

seqToList :: Sequ a -> [a]
seqToList (Sequ x) = x []

instance Show a => Show (Sequ a) where
    showsPrec d a = showsPrec d (seqToList a)
