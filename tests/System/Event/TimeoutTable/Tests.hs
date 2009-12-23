{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Event.TimeoutTable.Tests (tests) where

import           Data.List (nubBy, sortBy)
import qualified Data.Map as Map
import           Data.Maybe (isJust, fromJust, maybe)
import           Data.Time.Clock (UTCTime(..), addUTCTime)
import           Data.Time.Calendar (addDays, fromGregorian)
import           Data.Time.LocalTime ()
import qualified System.Event.TimeoutTable as TT
import qualified System.Event.TimeoutTable.Internal as TT

import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.QuickCheck (testProperty)
import           Test.QuickCheck (choose, (==>), Arbitrary(..), Property)

------------------------------------------------------------------------------
-- quickcheck instance for UTCTime -- need a superfluous newtype definition to
-- avoid an orphan Arbitrary instance

newtype UTM = UTM { unUTM :: UTCTime }
  deriving (Eq, Ord, Show)

instance Arbitrary UTM where
    arbitrary = do
        ndays <- choose (0,4)
        nsecs <- choose (0,86399)

        return $ UTM $ UTCTime (addDays ndays firstDay) (fromInteger nsecs)
      where
        firstDay = fromGregorian 2009 11 10

    coarbitrary = undefined


------------------------------------------------------------------------------
-- Arbitrary instance for (UTM,Int,Int)

newtype TVal = TVal (UTM,Int,Int)
  deriving (Eq, Show)

unTVal :: TVal -> (UTCTime, Int, Int)
unTVal (TVal ((UTM tm), a, b)) = (tm, a, b)

unTVals :: [TVal] -> [(UTCTime, Int, Int)]
unTVals = map unTVal

instance Arbitrary TVal where
    arbitrary = do
        tm <- arbitrary
        i1 <- arbitrary
        i2 <- arbitrary

        return $! TVal (tm,i1,i2)

    coarbitrary = undefined


------------------------------------------------------------------------------
-- tests follow
------------------------------------------------------------------------------
tests :: Test.Framework.Test
tests = testGroup "System.Event.TimeoutTable" testlist


testlist :: [Test]
testlist =
    [ testProperty "empty"             prop_empty
    , testProperty "show"              prop_show
    , testProperty "delete_empty"      prop_delete_empty
    , testProperty "handles_same_time" prop_handles_same_time
    , testProperty "singleton"         prop_singleton
    , testProperty "update"            prop_update
    , testProperty "null_update"       prop_null_update
    , testProperty "insert_delete"     prop_insert_delete
    , testProperty "oldest"            prop_oldest ]


prop_empty :: Bool
prop_empty = Map.null (TT._keySet tt) && Map.null (TT._timeSet tt)
  where
    !tt = TT.empty


-- mostly here for program coverage
prop_show :: Bool
prop_show = p1 && p2
  where
    !e  = TT.empty :: TT.TimeoutTable Int Int Int
    !e' = TT.insert 0 0 0 e

    !p1 = show e == "<TimeoutTable (fromList [])>"
    !p2 = show e' == "<TimeoutTable (fromList [(0,\"(0,0)\")])>"

prop_delete_empty :: Bool
prop_delete_empty = TT.null tt'
  where
    !tt  = TT.empty :: TT.TimeoutTable UTCTime Int Int
    !tt' = TT.delete 0 tt


prop_handles_same_time :: Bool
prop_handles_same_time = m1 && m2 && not m3 && m4 && m5
  where
    !e   = TT.empty :: TT.TimeoutTable Int Int ()
    !tt  = TT.insert 0 2 () $ TT.insert 0 1 () e

    !m1  = TT.member 1 tt
    !m2  = TT.member 2 tt

    !tt' = TT.delete 1 tt

    !m3  = TT.member 1 tt'
    !m4  = TT.member 2 tt'

    !tt'' = TT.delete 2 tt'
    !m5   = TT.null tt''


prop_singleton :: TVal -> Bool
prop_singleton (TVal (utm,k,v)) = member && p1 && p2
  where
    !tm     = unUTM utm
    !tt     = TT.insert tm k v TT.empty
    !member = TT.member k tt
    !oldest = TT.findOldest tt
    !mbval  = TT.find k tt

    !p1 = maybe False (\(tm',k',v') -> tm == tm' && k == k' && v == v') oldest
    !p2 = maybe False (\(tm',v') -> tm == tm' && v == v') mbval


prop_insert_delete :: (TVal, [TVal]) -> Bool
prop_insert_delete (TVal ((UTM tm),k,v), tvals) =
    and [ member, not memberAfter, p1, p2 ]
  where
    !vals                   = unTVals tvals
    !startTable             = TT.fromList vals
    !withElem               = TT.insert tm k v startTable
    !afterDelete            = TT.delete k withElem
    !member                 = TT.member k withElem
    !mbval                  = TT.find k withElem

    !memberAfter            = TT.member k afterDelete
    !valAfter               = TT.find k afterDelete

    !p1 = maybe False (\(tm',v') -> tm == tm' && v == v') mbval
    !p2 = not $ isJust valAfter


prop_oldest :: [TVal] -> Property
prop_oldest tvals = (not $ null vals) ==> p1
  where
    !tmpvals = unTVals tvals
    !vals    = nubBy tEQ tmpvals
    !sorted  = sortBy tCompare vals

    (!tm,!k,!v) = head sorted
    !tab        = TT.fromList vals
    !mbOldest   = TT.findOldest tab

    !p1 = maybe False (\(tm',k',v') -> tm == tm' && k == k' && v == v') mbOldest



tCompare :: (Ord t, Ord t1) => (t, t1, t2) -> (t, t1, t3) -> Ordering
tCompare (a,i,_) (b,j,_) = if a == b then compare i j else compare a b


tEQ :: (Eq t, Eq t1) => (t, t1, t2) -> (t, t1, t3) -> Bool
tEQ      (a,i,_) (b,j,_) = a == b || i == j


prop_update :: [TVal] -> Property
prop_update tvals = (not $ null vals) ==> p1 && p2 && p3
  where
    vals           = nubBy tEQ $ unTVals tvals
    sorted         = sortBy tCompare vals

    (tm,k,v)       = if null sorted then error (show vals) else head sorted
    tt             = TT.fromList vals

    tm'            = addUTCTime (-100) tm
    tt'            = TT.update k tm' tt
    (tm'',v')      = fromJust $ TT.find k tt'

    (tm''',k',v'') = fromJust $ TT.findOldest tt'

    newk           = k+1
    newv           = v+1

    tt''           = TT.update newk tm' $ TT.insert tm newk newv tt'

    (ft,fv)        = fromJust $ TT.find newk tt''

    p1             = tm'' == tm' && v == v'
    p2             = tm''' == tm' && v == v'' && k == k'
    p3             = ft == tm' && fv == newv


prop_null_update :: Bool
prop_null_update = TT.null $ TT.update 0 0 $ e
  where
    e = TT.empty :: TT.TimeoutTable Int Int ()
