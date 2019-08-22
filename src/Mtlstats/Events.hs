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

module Mtlstats.Events (handleEvent) where

import Control.Monad.Trans.State (StateT, gets, modify)
import Lens.Micro ((.~))
import Lens.Micro.Extras (view)
import qualified UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Menu
import Mtlstats.Types

-- | Event handler
handleEvent
  :: C.Event
  -- ^ The even being handled
  -> StateT ProgState C.Curses Bool
handleEvent e = do
  m <- gets $ view progMode
  case m of
    MainMenu  -> menuHandler mainMenu e
    NewSeason -> menuHandler newSeasonMenu e >> return True
    NewGame _ -> return True
