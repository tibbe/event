module Main (main) where

import Distribution.Simple (defaultMainWithHooks, runTests, simpleUserHooks)
import System.Cmd (system)

main :: IO ()
main =
    defaultMainWithHooks $ simpleUserHooks { runTests = runTests' }
  where
    runTests' _ _ _ _ = do
        system "./dist/build/test/test -j4"
        return ()
