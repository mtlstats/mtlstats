{- |

mtlstats
Copyright (C) 2019 Rhéal Lamothe
<rheal.lamothe@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

-}

module Mtlstats (initState, mainLoop) where

import Control.Monad (void)
import Control.Monad.Extra (whenM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (get, gets)
import Data.Maybe (fromJust)
import qualified UI.NCurses as C

import Mtlstats.Control
import Mtlstats.Types

-- | Initializes the progran
initState :: C.Curses ProgState
initState = do
  C.setEcho False
  void $ C.setCursorMode C.CursorInvisible
  return newProgState

-- | Main program loop
mainLoop :: Action ()
mainLoop = do
  c <- gets dispatch
  get >>= lift . draw . drawController c
  w <- lift C.defaultWindow
  whenM (lift (fromJust <$> C.getEvent w Nothing) >>= handleController c)
    mainLoop

draw :: C.Update C.CursorMode -> C.Curses ()
draw u = do
  void $ C.setCursorMode C.CursorInvisible
  w  <- C.defaultWindow
  cm <- C.updateWindow w $ do
    C.clear
    u
  C.render
  void $ C.setCursorMode cm
