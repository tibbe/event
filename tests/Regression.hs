module Regression (tests) where

import Control.Exception (bracket)
import Test.HUnit (Assertion, assertBool, assertEqual)
import qualified System.Event as E
import qualified Test.Framework as F
import qualified Test.Framework.Providers.HUnit as F

startupShutdown :: Assertion
startupShutdown = E.new >>= E.shutdown

tests :: F.Test
tests = F.testGroup "Regression" [
         F.testCase "startupShutdown" startupShutdown
        ]
