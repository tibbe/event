module Regression (tests) where

import Control.Exception (bracket)
import Test.HUnit (Assertion)
import qualified System.Event as E
import qualified Test.Framework as F
import qualified Test.Framework.Providers.HUnit as F

startupShutdown :: Assertion
startupShutdown = E.new >>= E.shutdown

doubleRegister :: Assertion
doubleRegister = bracket E.new E.shutdown $ \mgr -> do
  let cb _ _ = return ()
  _ <- E.registerFd_ mgr cb 1 E.evtRead
  _ <- E.registerFd_ mgr cb 1 E.evtRead
  return ()

tests :: F.Test
tests = F.testGroup "Regression" [
          F.testCase "doubleRegister" doubleRegister
        , F.testCase "startupShutdown" startupShutdown
        ]
