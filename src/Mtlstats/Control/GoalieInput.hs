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

module Mtlstats.Control.GoalieInput (goalieInput) where

import Data.Maybe (fromMaybe)
import Lens.Micro ((^.))
import qualified UI.NCurses as C

import Mtlstats.Format
import Mtlstats.Menu
import Mtlstats.Prompt
import Mtlstats.Prompt.GoalieInput
import Mtlstats.Types
import Mtlstats.Util

-- | The dispatcher for handling goalie input
goalieInput :: ProgState -> Controller
goalieInput s = let
  gs = s^.progMode.gameStateL
  in if gs^.gameGoaliesRecorded
    then selectGameGoalieC s
  else if null $ gs^.gameSelectedGoalie
    then selectGoalieC
  else if null $ gs^.gameGoalieMinsPlayed
    then minsPlayedC
  else goalsAllowedC

selectGoalieC :: Controller
selectGoalieC = promptController selectGameGoaliePrompt

minsPlayedC :: Controller
minsPlayedC = promptControllerWith header goalieMinsPlayedPrompt

goalsAllowedC :: Controller
goalsAllowedC = promptControllerWith header goalsAllowedPrompt

selectGameGoalieC :: ProgState -> Controller
selectGameGoalieC = menuController . gameGoalieMenu

header :: ProgState -> C.Update ()
header s = C.drawString $ unlines
  [ "*** GAME " ++ padNum 2 (s^.database.dbGames) ++ " ***"
  , fromMaybe "" $ do
    n <- s^.progMode.gameStateL.gameSelectedGoalie
    g <- nth n $ s^.database.dbGoalies
    Just $ goalieSummary g
  ]
