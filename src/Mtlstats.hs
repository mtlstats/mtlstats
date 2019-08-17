module Mtlstats (initState, mainLoop) where

import Control.Monad.Trans.State (StateT)
import qualified UI.NCurses as C

import Mtlstats.Types

-- | Initializes the progran
initState :: C.Curses ProgState
initState = do
  C.setEcho False
  return ProgState

-- | Main program loop
mainLoop :: StateT ProgState C.Curses ()
mainLoop = return ()
