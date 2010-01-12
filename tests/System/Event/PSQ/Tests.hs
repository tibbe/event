{-# LANGUAGE ScopedTypeVariables #-}

module System.Event.PSQ.Tests (tests) where

import System.Event.PSQ (Elem(..), PSQ)
import qualified System.Event.PSQ as Q
import System.Event.Unique (Unique(..))

import Control.Monad (liftM3)
import Data.Function (on)
import Data.Int (Int64)
import qualified Data.List as L
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck

instance Arbitrary Int64 where
    arbitrary = fromInteger `fmap` arbitrary
    coarbitrary = undefined

instance Arbitrary Unique where
    arbitrary = Unique `fmap` arbitrary
    coarbitrary = undefined

instance Arbitrary a => Arbitrary (Elem a) where
    arbitrary = liftM3 E arbitrary arbitrary arbitrary
    coarbitrary = undefined

instance Arbitrary a => Arbitrary (PSQ a) where
    arbitrary = Q.fromList `fmap` arbitrary
    coarbitrary = undefined

tests :: Test
tests = testGroup "System.Event.PSQ" testlist

testlist :: [Test]
testlist =
    [ testProperty "adjust" propAdjust
    , testProperty "atMost" propAtMost
    , testProperty "delete" propDelete
    , testProperty "insert" propInsert
    , testProperty "min" propMin
    ]

propAtMost :: Q.Prio -> [(Q.Key, Q.Prio, Int)] -> Bool
propAtMost pt es =
    let (vs, q') = Q.atMost pt q
    in (map toTuple vs, map toTuple (Q.toAscList q')) == atMost pt (fromList es)
  where
    q = Q.fromList $ map fromTuple es
    toTuple (E k p v) = (k, p, v)
    fromTuple (k, p, v) = E k p v

propAdjust :: Q.Key -> Q.Prio -> Int -> PSQ Int -> Q.Prio -> Property
propAdjust k p v q p' = p /= p' ==>
    case Q.lookup k (Q.adjust (const p') k (Q.insert k p v q)) of
        Just (p'', _) -> p' == p''
        _             -> False

propMin :: [(Q.Key, Q.Prio, Int)] -> Bool
propMin xs =
    case (findMin $ fromList xs, Q.findMin q) of
        (Nothing, Nothing)      -> True
        (Just (k, p, v), Just (E k' p' v')) ->
            k == k' && p == p' && v == v'
        _                                   -> False
  where q = Q.fromList . map (\(k, p, v) -> E k p v) $ xs

propInsert :: Q.Key -> Q.Prio -> Int -> PSQ Int -> Bool
propInsert k p v q =
    case Q.lookup k (Q.insert k p v q) of
        Just (p', v') -> p == p' && v == v'
        _             -> False

propDelete :: Q.Key -> Q.Prio -> Int -> PSQ Int -> Bool
propDelete k p v q =
    case Q.lookup k (Q.delete k (Q.insert k p v q)) of
        Just _ -> False
        _      -> True

------------------------------------------------------------------------
-- Simple priority queue model implemented using sorted lists.

-- A priority queue model. Keys are uniqueue and kept in ascending
-- order.
type Model a = [(Q.Key, Q.Prio, a)]

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

cmpKey :: (Q.Key, Q.Prio, a) -> (Q.Key, Q.Prio, a) -> Ordering
cmpKey = compare `on` fst3

cmpPrio :: (Q.Key, Q.Prio, a) -> (Q.Key, Q.Prio, a) -> Ordering
cmpPrio = compare `on` snd3

eqKey :: (Q.Key, Q.Prio, a) -> (Q.Key, Q.Prio, a) -> Bool
eqKey = (==) `on` fst3

insert :: Q.Key -> Q.Prio -> a -> Model a -> Model a
insert k p v q = L.insertBy cmpKey (k, p, v)
                 (L.deleteBy eqKey (k, undefined, undefined) q)

fromList :: [(Q.Key, Q.Prio, a)] -> Model a
fromList = foldr (\(k, p, v) q -> insert k p v q) []

findMin :: Model a -> Maybe (Q.Key, Q.Prio, a)
findMin [] = Nothing
findMin xs = Just $ L.minimumBy cmpPrio xs

atMost :: Q.Prio -> Model a -> ([(Q.Key, Q.Prio, a)], Model a)
atMost pt = L.partition (\e@(_, p, _) -> p <= pt)
