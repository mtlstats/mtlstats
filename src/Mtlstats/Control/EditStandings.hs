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

{-# LANGUAGE LambdaCase #-}

module Mtlstats.Control.EditStandings (editStandingsC) where

import Lens.Micro ((^.))
import qualified UI.NCurses as C

import Mtlstats.Format
import Mtlstats.Menu
import Mtlstats.Menu.EditStandings
import Mtlstats.Types

-- | Controller for the edit standings menu
editStandingsC :: EditStandingsMode -> Controller
editStandingsC = \case
  ESMMenu   -> menuControllerWith header editStandingsMenu
  ESMHome m -> editHomeStandingsC m
  ESMAway m -> editAwayStandingsC m

editHomeStandingsC :: ESMSubMode -> Controller
editHomeStandingsC = \case
  ESMSubMenu -> menuControllerWith header editHomeStandingsMenu
  _          -> undefined

editAwayStandingsC :: ESMSubMode -> Controller
editAwayStandingsC = \case
  ESMSubMenu -> menuControllerWith header editAwayStandingsMenu
  _          -> undefined

header :: ProgState -> C.Update ()
header = do
  db <- (^.database)
  let
    home  = db^.dbHomeGameStats
    away  = db^.dbAwayGameStats
    table = numTable ["   W", "   L", "  OT", "  GF", "  GA"]
      [ ( "HOME", valsFor home )
      , ( "ROAD", valsFor away )
      ]
  return $ C.drawString $ unlines $ table ++ [""]

valsFor :: GameStats -> [Int]
valsFor gs =
  [ gs^.gmsWins
  , gs^.gmsLosses
  , gs^.gmsOvertime
  , gs^.gmsGoalsFor
  , gs^.gmsGoalsAgainst
  ]
