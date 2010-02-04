module Array (tests) where

import System.Event.Array (Array)
import qualified System.Event.Array as A

import Test.Framework (testGroup)
import qualified Test.Framework
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit (Test(..), (@=?))


tests :: Test.Framework.Test
tests = testGroup "System.Event.Array" $ hUnitTestToTests testlist


testlist :: Test
testlist = TestList
      [ TestLabel "empty"   testEmpty
      , TestLabel "new 0"   (testNew   0   0)
      , TestLabel "new 1"   (testNew   1   1)
      , TestLabel "new 50"  (testNew  50  64)
      , TestLabel "new 100" (testNew 100 128)
      , TestLabel "new 128" (testNew 128 128)
      , TestLabel "ensureCapacity 0 on empty" (testEnsureCapacity A.empty   0 0)
      , TestLabel "ensureCapacity 0 on cap 4" (testEnsureCapacity (A.new 4) 0 4)
      , TestLabel "ensureCapacity 4 on cap 4" (testEnsureCapacity (A.new 4) 4 4)
      , TestLabel "ensureCapacity 5 on cap 4" (testEnsureCapacity (A.new 4) 5 8)
      , TestLabel "1 snoc on cap 0"  (testSnoc 0 1 1)
      , TestLabel "5 snocs on cap 4" (testSnoc 4 5 8)
      ]


testEmpty :: Test
testEmpty = TestCase $ do
    arr  <- A.empty
    lenA <- A.length   arr
    capA <- A.capacity arr
    0 @=? lenA
    0 @=? capA

testNew :: Int -> Int -> Test
testNew cI cC = TestCase $ do
    arr  <- A.new cI :: IO (Array Int)
    lenA <- A.length   arr
    capA <- A.capacity arr
    0  @=? lenA
    cC @=? capA

testEnsureCapacity :: IO (Array Int) -> Int -> Int -> Test
testEnsureCapacity ioarr cI cC = TestCase $ do
    arr <- ioarr
    A.ensureCapacity arr cI
    lenA <- A.length   arr
    capA <- A.capacity arr
    0  @=? lenA
    cC @=? capA

testSnoc :: Int -> Int -> Int -> Test
testSnoc cI h capC = TestCase $ do
    arr <- A.new cI :: IO (Array Int)
    mapM_ (A.snoc arr) esC
    lenA <- A.length   arr
    capA <- A.capacity arr
    esA  <- mapM (A.unsafeRead arr) is
    lenC @=? lenA
    capC @=? capA
    esC  @=? esA
  where
    lenC = h
    esC  = [1..h]
    is   = [0..pred h]
