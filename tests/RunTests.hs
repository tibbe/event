import qualified System.Event.Array.Tests
import qualified System.Event.PSQ.Tests

import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain tests
  where tests = [ System.Event.Array.Tests.tests
                , System.Event.PSQ.Tests.tests
                ]
