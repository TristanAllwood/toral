import Test.Tasty

import HeapTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [heapTests]
