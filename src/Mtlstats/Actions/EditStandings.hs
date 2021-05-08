{- |

mtlstats
Copyright (C) 1984, 1985, 2019, 2020, 2021 Rhéal Lamothe
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

module Mtlstats.Actions.EditStandings
  ( editStandings
  , editHomeStandings
  , editAwayStandings
  , editWins
  , editLosses
  , editOvertime
  , editGoalsFor
  , editGoalsAgainst
  ) where

import Lens.Micro ((.~))

import Mtlstats.Types

-- | Enters edit standings mode
editStandings :: ProgState -> ProgState
editStandings = progMode .~ EditStandings ESMMenu

-- | Edits the home standings
editHomeStandings :: ProgState -> ProgState
editHomeStandings = progMode .~ EditStandings (ESMHome ESMSubMenu)

-- | Edits the road standings
editAwayStandings :: ProgState -> ProgState
editAwayStandings = progMode .~ EditStandings (ESMAway ESMSubMenu)

-- | Changes to edit wins mode
editWins :: ProgState -> ProgState
editWins = doEdit ESMEditWins

-- | Changes to edit losses mode
editLosses :: ProgState -> ProgState
editLosses = doEdit ESMEditLosses

-- | Changes to edit overtime mode
editOvertime :: ProgState -> ProgState
editOvertime = doEdit ESMEditOvertime

-- | Changes to edit goals for mode
editGoalsFor :: ProgState -> ProgState
editGoalsFor = doEdit ESMEditGoalsFor

-- | Changes to edit goals against mode
editGoalsAgainst :: ProgState -> ProgState
editGoalsAgainst = doEdit ESMEditGoalsAgainst

doEdit :: ESMSubMode -> ProgState -> ProgState
doEdit = (progMode.editStandingsModeL.esmSubModeL .~)
