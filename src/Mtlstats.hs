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
