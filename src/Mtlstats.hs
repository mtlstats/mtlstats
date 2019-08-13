module Mtlstats (initState, mainLoop) where

import Control.Monad.Trans.State (StateT)
import UI.NCurses (Curses)

import Mtlstats.Types

initState :: Curses ProgState
initState = return ProgState

mainLoop :: StateT ProgState Curses ()
mainLoop = return ()
