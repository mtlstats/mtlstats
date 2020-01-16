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

module Mtlstats.Menu.EditStandings
  ( editStandingsMenu
  , editHomeStandingsMenu
  , editAwayStandingsMenu
  ) where

import Control.Monad.Trans.State (modify)

import Mtlstats.Actions
import Mtlstats.Actions.EditStandings
import Mtlstats.Types.Menu

editStandingsMenu :: Menu ()
editStandingsMenu = Menu "*** EDIT STANDINGS ***" ()
  [ MenuItem '1' "Edit home standings" $
    modify editHomeStandings
  , MenuItem '2' "Edit road standings" $
    modify editAwayStandings
  , MenuItem 'R' "Return to main menu" $
    modify backHome
  ]

editHomeStandingsMenu :: Menu ()
editHomeStandingsMenu = subMenu "HOME"

editAwayStandingsMenu :: Menu ()
editAwayStandingsMenu = subMenu "ROAD"

subMenu :: String -> Menu ()
subMenu = undefined
