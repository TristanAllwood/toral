module Main where

import Graphics.Vty

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
    _ -> mainLoop (handleEvent e s) vty
