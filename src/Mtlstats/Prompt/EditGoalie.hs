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

module Mtlstats.Prompt.EditGoalie
  ( goalieToEditPrompt
  , editGoalieNumberPrompt
  , editGoalieNamePrompt
  , editGoalieYtdGamesPrompt
  , editGoalieYtdMinsPrompt
  , editGoalieYtdGoalsPrompt
  , editGoalieYtdWinsPrompt
  , editGoalieYtdLossesPrompt
  , editGoalieYtdTiesPrompt
  , editGoalieLtGamesPrompt
  ) where

import Control.Monad.Trans.State (modify)
import Lens.Micro ((.~))

import Mtlstats.Actions.EditGoalie
import Mtlstats.Prompt
import Mtlstats.Types

-- | Prompt to select a 'Goalie' for editing
goalieToEditPrompt :: Prompt
goalieToEditPrompt = selectGoaliePrompt "Goalie to edit: " $
  modify . (progMode.editGoalieStateL.egsSelectedGoalie .~)

-- | Prompt to edit a goalie's number
editGoalieNumberPrompt :: Prompt
editGoalieNumberPrompt = numPrompt "Goalie number: " $
  modify . editGoalieNumber

-- | Prompt to edit a goalie's name
editGoalieNamePrompt :: Prompt
editGoalieNamePrompt = strPrompt "Goalie name: " $
  modify . editGoalieName

-- | Prompt to edit a goalie's YTD games played
editGoalieYtdGamesPrompt :: Prompt
editGoalieYtdGamesPrompt = numPrompt "Year-to-date games played: " $
  modify . editGoalieYtdGames

-- | Prompt to edit a goalie's YTD minutes played
editGoalieYtdMinsPrompt :: Prompt
editGoalieYtdMinsPrompt = numPrompt "Year-to-date minutes played: " $
  modify . editGoalieYtdMins

-- | Prompt to edit a goalie's YTD goales allowed
editGoalieYtdGoalsPrompt :: Prompt
editGoalieYtdGoalsPrompt = numPrompt "Year-to-date goals allowed: " $
  modify . editGoalieYtdGoals

-- | Prompt to edit a goalie's YTD wins
editGoalieYtdWinsPrompt :: Prompt
editGoalieYtdWinsPrompt = numPrompt "Year-to-date wins: " $
  modify . editGoalieYtdWins

-- | Prompt to edit a goalie's YTD losses
editGoalieYtdLossesPrompt :: Prompt
editGoalieYtdLossesPrompt = numPrompt "Year-to-date losses: " $
  modify . editGoalieYtdLosses

-- | Prompt to edit a goalie's YTD ties
editGoalieYtdTiesPrompt :: Prompt
editGoalieYtdTiesPrompt = numPrompt "Year-to-date ties: " $
  modify . editGoalieYtdTies

-- | Prompt to edit a goalie's lifetime games played
editGoalieLtGamesPrompt :: Prompt
editGoalieLtGamesPrompt = undefined
