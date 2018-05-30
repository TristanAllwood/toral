module AppView where

import Graphics.Vty

import Lib
import Heap.Heap

class AppView a where
  render :: RenderOpts -> a -> (Heap ProgramState) -> Image

data RenderOpts
  = RenderOpts
  { hasWindowFocus :: Bool
  , width          :: Int
  , height         :: Int
  }

data ScrollWindow a
  = ScrollWindow
  { child :: a
  , xScrollPosition :: Int
  , yScrollPosition :: Int
  }

instance AppView a => AppView (ScrollWindow a) where
  render renderOpts sw heap = render renderOpts (child sw) heap

