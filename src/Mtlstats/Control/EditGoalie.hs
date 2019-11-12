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

import Lens.Micro ((^.))

import Mtlstats.Prompt
import Mtlstats.Prompt.EditGoalie
import Mtlstats.Types

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
menuC = undefined

numberC :: Controller
numberC = undefined

nameC :: Controller
nameC = undefined

ytdGamesC :: Controller
ytdGamesC = undefined

ytdMinsC :: Controller
ytdMinsC = undefined

ytdGoalsC :: Controller
ytdGoalsC = undefined

ytdWinsC :: Controller
ytdWinsC = undefined

ytdLossesC :: Controller
ytdLossesC = undefined

ytdTiesC :: Controller
ytdTiesC = undefined

ltGamesC :: Controller
ltGamesC = undefined

ltMinsC :: Controller
ltMinsC = undefined

ltGoalsC :: Controller
ltGoalsC = undefined

ltWinsC :: Controller
ltWinsC = undefined

ltLossesC :: Controller
ltLossesC = undefined

ltTiesC :: Controller
ltTiesC = undefined
