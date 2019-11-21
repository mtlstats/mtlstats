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

{-# LANGUAGE LambdaCase #-}

module Mtlstats.Control.EditGoalie (editGoalieC) where

import Data.Maybe (fromMaybe)
import Lens.Micro ((^.))
import UI.NCurses as C

import Mtlstats.Helpers.Goalie
import Mtlstats.Menu
import Mtlstats.Menu.EditGoalie
import Mtlstats.Prompt
import Mtlstats.Prompt.EditGoalie
import Mtlstats.Types
import Mtlstats.Util

-- | Controller/dispatcher for editing a 'Goalie'
editGoalieC :: EditGoalieState -> Controller
editGoalieC egs
  | null $ egs^.egsSelectedGoalie = selectC
  | otherwise = editC $ egs^.egsMode

selectC :: Controller
selectC = promptController goalieToEditPrompt

editC :: EditGoalieMode -> Controller
editC = \case
  EGMenu      -> menuC
  EGNumber    -> numberC
  EGName      -> nameC
  EGYtd       -> ytdMenuC
  EGLifetime  -> lifetimeMenuC
  EGYtdGames  -> ytdGamesC
  EGYtdMins   -> ytdMinsC
  EGYtdGoals  -> ytdGoalsC
  EGYtdWins   -> ytdWinsC
  EGYtdLosses -> ytdLossesC
  EGYtdTies   -> ytdTiesC
  EGLtGames   -> ltGamesC
  EGLtMins    -> ltMinsC
  EGLtGoals   -> ltGoalsC
  EGLtWins    -> ltWinsC
  EGLtLosses  -> ltLossesC
  EGLtTies    -> ltTiesC

menuC :: Controller
menuC = menuControllerWith header editGoalieMenu

numberC :: Controller
numberC = promptController editGoalieNumberPrompt

nameC :: Controller
nameC = promptController editGoalieNamePrompt

ytdMenuC :: Controller
ytdMenuC = menuControllerWith header editGoalieYtdMenu

lifetimeMenuC :: Controller
lifetimeMenuC = menuControllerWith header editGoalieLtMenu

ytdGamesC :: Controller
ytdGamesC = promptController editGoalieYtdGamesPrompt

ytdMinsC :: Controller
ytdMinsC = promptController editGoalieYtdMinsPrompt

ytdGoalsC :: Controller
ytdGoalsC = promptController editGoalieYtdGoalsPrompt

ytdWinsC :: Controller
ytdWinsC = promptController editGoalieYtdWinsPrompt

ytdLossesC :: Controller
ytdLossesC = promptController editGoalieYtdLossesPrompt

ytdTiesC :: Controller
ytdTiesC = promptController editGoalieYtdTiesPrompt

ltGamesC :: Controller
ltGamesC = promptController editGoalieLtGamesPrompt

ltMinsC :: Controller
ltMinsC = promptController editGoalieLtMinsPrompt

ltGoalsC :: Controller
ltGoalsC = promptController editGoalieLtGoalsPrompt

ltWinsC :: Controller
ltWinsC = promptController editGoalieLtWinsPrompt

ltLossesC :: Controller
ltLossesC = promptController editGoalieLtLossesPrompt

ltTiesC :: Controller
ltTiesC = promptController editGoalieLtTiesPrompt

header :: ProgState -> C.Update ()
header s = C.drawString $ fromMaybe "" $ do
  gid <- s^.progMode.editGoalieStateL.egsSelectedGoalie
  g   <- nth gid $ s^.database.dbGoalies
  Just $ goalieDetails g ++ "\n"
