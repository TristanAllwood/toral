{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module Lib
    ( someFunc
    , AppState
    , initAppState
    , Action(..)

    , renderState
    , handleEvent

    ) where

import qualified Graphics.Vty as V
import Graphics.Vty (Event, Image, (<->))

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

renderState :: AppState -> Image
renderState _ = line0 <-> line1
  where
    line0 = V.string (V.defAttr `V.withForeColor` V.green) "first line"
    line1 = V.string (V.defAttr `V.withBackColor` V.blue) "second line"

handleEvent :: Event -> AppState -> AppState
handleEvent _ = id

data Action (id :: * -> *)
  = CreateFunction (id Function)
  | EditFunction (id Function)
  | NewDataType (id DataType)
  | EditDataType (id DataType)

data Function
data DataType
