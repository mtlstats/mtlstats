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

module Mtlstats.Prompt.EditStandings
  ( editHomeWinsPrompt
  , editHomeLossesPrompt
  , editHomeOvertimePrompt
  , editHomeGoalsForPrompt
  , editHomeGoalsAgainstPrompt
  , editAwayWinsPrompt
  , editAwayLossesPrompt
  , editAwayOvertimePrompt
  , editAwayGoalsForPrompt
  , editAwayGoalsAgainstPrompt
  ) where

import Control.Monad.Trans.State (modify)
import Lens.Micro ((.~), (%~))

import Mtlstats.Prompt
import Mtlstats.Types

editHomeWinsPrompt :: Prompt
editHomeWinsPrompt =
  mkPrompt "Home wins: " (dbHomeGameStats.gmsWins .~)

editHomeLossesPrompt :: Prompt
editHomeLossesPrompt =
  mkPrompt "Home losses: " (dbHomeGameStats.gmsLosses .~)

editHomeOvertimePrompt :: Prompt
editHomeOvertimePrompt =
  mkPrompt "Home overtime games: " (dbHomeGameStats.gmsOvertime .~)

editHomeGoalsForPrompt :: Prompt
editHomeGoalsForPrompt =
  mkPrompt "Home goals for: " (dbHomeGameStats.gmsGoalsFor .~)

editHomeGoalsAgainstPrompt :: Prompt
editHomeGoalsAgainstPrompt =
  mkPrompt "Home goals against: " (dbHomeGameStats.gmsGoalsAgainst .~)

editAwayWinsPrompt :: Prompt
editAwayWinsPrompt =
  mkPrompt "Road wins: " (dbAwayGameStats.gmsWins .~)

editAwayLossesPrompt :: Prompt
editAwayLossesPrompt =
  mkPrompt "Road losses: " (dbAwayGameStats.gmsLosses .~)

editAwayOvertimePrompt :: Prompt
editAwayOvertimePrompt =
  mkPrompt "Road overtime games: " (dbAwayGameStats.gmsOvertime .~)

editAwayGoalsForPrompt :: Prompt
editAwayGoalsForPrompt =
  mkPrompt "Road goals for: " (dbAwayGameStats.gmsGoalsFor .~)

editAwayGoalsAgainstPrompt :: Prompt
editAwayGoalsAgainstPrompt =
  mkPrompt "Road goals against: " (dbAwayGameStats.gmsGoalsAgainst .~)

mkPrompt :: String -> (Int -> Database -> Database) -> Prompt
mkPrompt pStr f = numPromptWithFallback pStr
  (modify subMenu)
  (\n -> modify
    $ (database %~ f n)
    . subMenu)

subMenu :: ProgState -> ProgState
subMenu = progMode.editStandingsModeL.esmSubModeL .~ ESMSubMenu
