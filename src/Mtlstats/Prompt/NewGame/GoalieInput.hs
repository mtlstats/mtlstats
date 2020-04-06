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

module Mtlstats.Prompt.NewGame.GoalieInput
  ( selectGameGoaliePrompt
  , goalieMinsPlayedPrompt
  , goalsAllowedPrompt
  ) where

import Control.Monad.Trans.State (modify)
import Lens.Micro ((?~))

import Mtlstats.Actions.NewGame.GoalieInput
import Mtlstats.Prompt
import Mtlstats.Types

-- | Prompts for a goalie who played in the game
selectGameGoaliePrompt :: Prompt
selectGameGoaliePrompt = selectActiveGoaliePrompt
  "Which goalie played this game: " $
  \case
    Nothing -> modify finishGoalieEntry
    Just n  -> modify $ progMode.gameStateL.gameSelectedGoalie  ?~ n

-- | Prompts for the number of minutes the goalie has played
goalieMinsPlayedPrompt :: Prompt
goalieMinsPlayedPrompt = numPrompt "Minutes played: " $
  modify . (progMode.gameStateL.gameGoalieMinsPlayed ?~)

-- | Prompts for the number of goals the goalie allowed
goalsAllowedPrompt :: Prompt
goalsAllowedPrompt = numPrompt "Goals allowed: " $ \n -> do
  modify (progMode.gameStateL.gameGoalsAllowed ?~ n)
  modify recordGoalieStats
