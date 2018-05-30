
import Test.Tasty
import Test.Tasty.HUnit


commandTests :: TestTree
commandTests = testGroup "Command Tests"
  [ commandTest1
  ]

commandTest1 :: TestTree
commandTest1 = testGroup "Command Test 1" $ do
  testRunProgramState initProgramState $ do

    fid <- action CreateFunction
    ()  <- action (SetName fid "ID")
    dl  <- action (AddDefintionLine fid)
    arg <- action (AddArgument dl)
    res <- action (SetResult dl (ArgRef arg)

    TODO "Write some checks!"
