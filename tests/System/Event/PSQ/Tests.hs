{-# LANGUAGE ScopedTypeVariables #-}

module System.Event.PSQ.Tests (tests) where

import System.Event.PSQ (Elem(..), PSQ)
import qualified System.Event.PSQ as Q

import Data.Function (on)
import qualified Data.List as L
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck

instance Arbitrary a => Arbitrary (PSQ a) where
    arbitrary = do
        ks <- arbitrary
        ps <- arbitrary
        vs <- arbitrary
        return . Q.fromList $ zipWith3 E ks ps vs

tests :: Test
tests = testGroup "System.Event.PSQ"  testlist

testlist :: [Test]
testlist =
    [ testProperty "delete" propDelete
    , testProperty "insert" propInsert
    , testProperty "min" propMin
    ]

propMin (xs :: [(Q.Key, Q.Prio, Int)]) =
    case (findMin $ fromList xs, Q.findMin q) of
        (Nothing, Nothing)      -> True
        (Just (k, p, v), Just (E k' p' v')) ->
            k == k' && p == p' && v == v'
        _                                   -> False
  where q = Q.fromList . map (\(k, p, v) -> E k p v) $ xs

propInsert k p (v :: Int) q =
    case Q.lookup k (Q.insert k p v q) of
        Just (p', v') -> p == p' && v == v'
        _             -> False

propDelete k p (v :: Int) q =
    case Q.lookup k (Q.delete k (Q.insert k p v q)) of
        Just _ -> False
        _      -> True

------------------------------------------------------------------------
-- Simple priority queue model implemented using sorted lists.

-- A priority queue model. Keys are uniqueue and kept in ascending
-- order.
type Model a = [(Q.Key, Q.Prio, a)]

fst3 (x, _, _) = x
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
