{-# LANGUAGE ScopedTypeVariables #-}

module System.Event.PSQ.Tests (tests) where

import System.Event.PSQ (Elem(..), PSQ)
import qualified System.Event.PSQ as Q

import Data.Function (on)
import qualified Data.List as L
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck

instance Arbitrary PSQ where
    arbitrary = do
        ks <- arbitrary
        ps <- arbitrary
        return . Q.fromList $ zipWith E ks ps

tests :: Test
tests = testGroup "System.Event.PSQ"  testlist

testlist :: [Test]
testlist =
    [ testProperty "delete" propDelete
    , testProperty "insert" propInsert
    , testProperty "min" propMin
    ]

propMin xs =
    case (findMin $ fromList xs, Q.findMin q) of
        (Nothing, Nothing)      -> True
        (Just p, Just (E k p')) -> p == p'
        _                       -> False
  where q = Q.fromList . map (\(k, p) -> E k p) $ xs

propInsert k p q =
    case Q.lookup k (Q.insert k p q) of
        Just p' -> p == p'
        _       -> False

propDelete k p q =
    case Q.lookup k (Q.delete k (Q.insert k p q)) of
        Just _ -> False
        _      -> True

------------------------------------------------------------------------
-- Simple priority queue model implemented using sorted lists.

-- A priority queue model. Keys are uniqueue and kept in ascending
-- order.
type Model = [(Q.Key, Q.Prio)]

cmpKey :: (Q.Key, Q.Prio) -> (Q.Key, Q.Prio) -> Ordering
cmpKey = compare `on` fst

eqKey :: (Q.Key, Q.Prio) -> (Q.Key, Q.Prio) -> Bool
eqKey = (==) `on` fst

insert :: Q.Key -> Q.Prio -> Model -> Model
insert k p q = L.insertBy cmpKey (k, p) (L.deleteBy eqKey (k, undefined) q)

fromList :: [(Q.Key, Q.Prio)] -> Model
fromList = foldr (\(k, p) q -> insert k p q) []

findMin :: Model -> Maybe Q.Prio
findMin [] = Nothing
findMin xs = Just $ L.minimum (map snd xs)
