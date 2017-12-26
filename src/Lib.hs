{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module Lib
    ( someFunc
    , AppState
    , initAppState
    , Action(..)
    ) where

import Heap.Heap

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data AppState
  = AppState {
    text :: String,
    programState :: Heap ProgramState
  }

data ProgramState s
  = ProgramState {
    commands :: [ Action (Ref s) ]
  }

initAppState :: AppState
initAppState
  = AppState {
      text = "Hello",
      programState = newHeap initProgramState
  }

initProgramState :: forall s . ProgramState s
initProgramState = ProgramState { commands = [] }

data Action (id :: * -> *)
  = CreateFunction (id Function)
  | EditFunction (id Function)
  | NewDataType (id DataType)
  | EditDataType (id DataType)

data Function
data DataType
