{-# LANGUAGE RankNTypes #-}
module HeapTest (
  heapTests
)  where

import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit

import Heap.Heap

heapTests :: TestTree
heapTests = testGroup "Heap Tests"
  [ testSimpleWorldTwoStep
  , testWorldTest1
  ]


data SimpleTestWorld w
  = SimpleTestWorld
  { simplePointer :: Maybe (Ref w Int)
  }

testSimpleWorldTwoStep :: TestTree
testSimpleWorldTwoStep = testCase "SimpleWorldTests" $ do
    assertBool "isJust in new world" (isJust . simplePointer . getWorld $ heap')
    10 @?= viewHeap heap' (\rr -> (maybe (-1) rr . simplePointer))
  where
    heap = newHeap (SimpleTestWorld { simplePointer = Nothing })
    heap' = runHeap heap $ \stw -> do
              ref <- newRef 10
              return stw { simplePointer = Just ref }


data TestWorld w
  = TestWorld
  { intPointer1 :: Ref w Int
  , intPointer2 :: Ref w Int
  , nestedPointer :: Ref w (Ref w Int)
  }

ptr1  :: Heap TestWorld -> Int
ptr1 = ptr intPointer1

ptr2  :: Heap TestWorld -> Int
ptr2 = ptr intPointer2

ptr :: (forall w. TestWorld w -> Ref w Int) -> Heap TestWorld -> Int
ptr p h = viewHeap h (\rr -> rr . p)

nested :: Heap TestWorld -> Int
nested h = viewHeap h (\rr -> rr . rr . nestedPointer)


testWorldTest1 :: TestTree
testWorldTest1 = testCase "Test World 1" $ do
  let heap1 = runNewHeap $ do
                r1 <- newRef 10
                r2 <- newRef 20
                np <- newRef r1
                return TestWorld { intPointer1 = r1,
                                   intPointer2 = r2,
                                   nestedPointer = np
                                 }

  let checks1 = do
                  10 @=? ptr1 heap1
                  20 @=? ptr2 heap1
                  10 @=? nested heap1
  checks1

  let heap2 = runHeap heap1 $ \w -> do
                writeRef (intPointer1 w) 30
                return w

  let checks2 = do
                  30 @=? ptr1 heap2
                  20 @=? ptr2 heap2
                  30 @=? nested heap2

  checks1
  checks2

  let heap3 = runHeap heap2 $ \w -> do
                writeRef (intPointer2 w) 40
                return w

  let checks3 = do
                  30 @=? ptr1 heap3
                  40 @=? ptr2 heap3
                  30 @=? nested heap3

  checks1
  checks2
  checks3

  let heap4 = runHeap heap3 $ \w -> do
                modifyRefM (intPointer1 w) (\x -> return (x*2))
                return w

  let checks4 = do
                  60 @=? ptr1 heap4
                  40 @=? ptr2 heap4
                  60 @=? nested heap4

  checks1
  checks2
  checks3
  checks4
