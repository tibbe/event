import qualified Array
import qualified PSQ

import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain tests
  where tests = [ Array.tests
                , PSQ.tests
                ]
