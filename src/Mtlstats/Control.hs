{- |

mtlstats
Copyright (C) 1984, 1985, 2019, 2020 Rhéal Lamothe
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

module Mtlstats.Control (dispatch) where

import Lens.Micro ((^.))

import Mtlstats.Control.CreateGoalie
import Mtlstats.Control.CreatePlayer
import Mtlstats.Control.EditGoalie
import Mtlstats.Control.EditPlayer
import Mtlstats.Control.EditStandings
import Mtlstats.Control.NewGame
import Mtlstats.Control.TitleScreen
import Mtlstats.Menu
import Mtlstats.Prompt
import Mtlstats.Types

-- | Reads the program state and returs the apropriate controller to
-- run
dispatch :: ProgState -> Controller
dispatch s = case s^.progMode of
  TitleScreen         -> titleScreenC
  MainMenu            -> mainMenuC
  NewSeason flag      -> newSeasonC flag
  NewGame gs          -> newGameC gs
  EditMenu            -> editMenuC
  CreatePlayer cps    -> createPlayerC cps
  CreateGoalie cgs    -> createGoalieC cgs
  EditPlayer eps      -> editPlayerC eps
  EditGoalie egs      -> editGoalieC egs
  (EditStandings esm) -> editStandingsC esm

mainMenuC :: Controller
mainMenuC = Controller
  { drawController   = const $ drawMenu mainMenu
  , handleController = menuHandler mainMenu
  }

newSeasonC :: Bool -> Controller
newSeasonC False = promptController newSeasonPrompt
newSeasonC True  = menuController newSeasonMenu

editMenuC :: Controller
editMenuC = menuController editMenu
