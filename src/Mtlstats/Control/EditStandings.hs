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

module Mtlstats.Control.EditStandings (editStandingsC) where

import Lens.Micro ((^.))
import qualified UI.NCurses as C

import Mtlstats.Format
import Mtlstats.Menu
import Mtlstats.Menu.EditStandings
import Mtlstats.Types

-- | Controller for the edit standings menu
editStandingsC :: Controller
editStandingsC = menuControllerWith header editStandingsMenu

header :: ProgState -> C.Update ()
header = do
  db <- (^.database)
  let
    home  = db^.dbHomeGameStats
    away  = db^.dbAwayGameStats
    table = numTable ["Wins", "Losses", "Overtime", "Goals for", "Goals against"]
      [ ( "Home", valsFor home )
      , ( "Road", valsFor away )
      ]
  return $ C.drawString $ unlines $ table ++ [""]

valsFor :: GameStats -> [Int]
valsFor = undefined
