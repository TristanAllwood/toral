module Main where

import Graphics.Vty hiding (text)
import qualified Graphics.Vty as V

import CommandView
import CodeView
import Heap.Heap
import Lib

main :: IO ()
main = do
  cfg <- standardIOConfig
  vty <- mkVty cfg

  let s = initAppState
  mainLoop s vty


mainLoop :: AppState -> Vty -> IO ()
mainLoop s vty = do
  let pic = picForImage (renderState s)
  update vty pic
  e <- nextEvent vty
  case e of
    EvKey KEsc [] -> shutdown vty
    EvKey (KChar 'q') [] -> shutdown vty
    EvKey (KChar 'Q') [] -> shutdown vty
    _ -> mainLoop (handleEvent e s) vty

renderState :: AppState -> Image
renderState _ = line0 <-> line1
  where
    line0 = V.string (V.defAttr `V.withForeColor` V.green) "first line"
    line1 = V.string (V.defAttr `V.withBackColor` V.blue) "second line"

handleEvent :: Event -> AppState -> AppState
handleEvent _ = id

data AppState
  = AppState {
    codeView :: CodeView,
    commandView :: CommandView,
    programState :: Heap ProgramState
  }

initAppState :: AppState
initAppState
  = AppState {
      codeView    = newCodeView,
      commandView = newCommandView,
      programState = newHeap initProgramState
  }
