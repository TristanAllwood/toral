{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module Lib
    ( ProgramState
    , Action(..)
    , initProgramState
    ) where

import Heap.Heap

data ProgramState s
  = ProgramState {
    commands :: [ Action (Ref s) ]
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
