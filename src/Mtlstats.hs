module Mtlstats (initState, mainLoop) where

import Control.Monad.Trans.State (StateT)
import qualified UI.NCurses as C

import Mtlstats.Types

-- | Initializes the progran
initState :: C.Curses ProgState
initState = do
  w <- C.defaultWindow
  C.updateWindow w C.clear
  C.render
  return ProgState

-- | Main program loop
mainLoop :: StateT ProgState C.Curses ()
mainLoop = return ()
