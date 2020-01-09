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
  , editGoalieYtdShutoutsPrompt
  , editGoalieYtdWinsPrompt
  , editGoalieYtdLossesPrompt
  , editGoalieYtdTiesPrompt
  , editGoalieLtGamesPrompt
  , editGoalieLtMinsPrompt
  , editGoalieLtGoalsPrompt
  , editGoalieLtShutoutsPrompt
  , editGoalieLtWinsPrompt
  , editGoalieLtLossesPrompt
  , editGoalieLtTiesPrompt
  ) where

import Control.Monad.Trans.State (modify)
import Lens.Micro ((.~))

import Mtlstats.Actions
import Mtlstats.Prompt
import Mtlstats.Types

-- | Prompt to select a 'Goalie' for editing
goalieToEditPrompt :: Prompt
goalieToEditPrompt = selectGoaliePrompt "Goalie to edit: " $
  modify . (progMode.editGoalieStateL.egsSelectedGoalie .~)

-- | Prompt to edit a goalie's number
editGoalieNumberPrompt :: Prompt
editGoalieNumberPrompt = editNum "Goalie number: " EGMenu
  (gNumber .~)

-- | Prompt to edit a goalie's name
editGoalieNamePrompt :: Prompt
editGoalieNamePrompt = namePrompt "Goalie name: " $ \name ->
  if null name
  then goto EGMenu
  else doEdit EGMenu $ gName .~ name

-- | Prompt to edit a goalie's YTD games played
editGoalieYtdGamesPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Prompt
editGoalieYtdGamesPrompt batchMode =
  editNum "Year-to-date games played: " mode
  (gYtd.gsGames .~)
  where
    mode = if batchMode then EGYtdMins True else EGYtd

-- | Prompt to edit a goalie's YTD minutes played
editGoalieYtdMinsPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Prompt
editGoalieYtdMinsPrompt batchMode =
  editNum "Year-to-date minutes played: " mode
  (gYtd.gsMinsPlayed .~)
  where
    mode = if batchMode then EGYtdGoals True else EGYtd

-- | Prompt to edit a goalie's YTD goales allowed
editGoalieYtdGoalsPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Prompt
editGoalieYtdGoalsPrompt batchMode =
  editNum "Year-to-date goals allowed: " mode
  (gYtd.gsGoalsAllowed .~)
  where
    mode = if batchMode then EGYtdShutouts True else EGYtd

-- | Prompt to edit a goalie's YTD shutouts
editGoalieYtdShutoutsPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Prompt
editGoalieYtdShutoutsPrompt batchMode =
  editNum "Year-to-date shutouts: " mode
  (gYtd.gsShutouts .~)
  where
    mode = if batchMode then EGYtdWins True else EGYtd

-- | Prompt to edit a goalie's YTD wins
editGoalieYtdWinsPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Prompt
editGoalieYtdWinsPrompt batchMode =
  editNum "Year-to-date wins: " mode
  (gYtd.gsWins .~)
  where
    mode = if batchMode then EGYtdLosses True else EGYtd

-- | Prompt to edit a goalie's YTD losses
editGoalieYtdLossesPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Prompt
editGoalieYtdLossesPrompt batchMode =
  editNum "Year-to-date losses: " mode
  (gYtd.gsLosses .~)
  where
    mode = if batchMode then EGYtdTies else EGYtd

-- | Prompt to edit a goalie's YTD ties
editGoalieYtdTiesPrompt :: Prompt
editGoalieYtdTiesPrompt = editNum "Year-to-date ties: " EGYtd
  (gYtd.gsTies .~)

-- | Prompt to edit a goalie's lifetime games played
editGoalieLtGamesPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Prompt
editGoalieLtGamesPrompt batchMode =
  editNum "Lifetime games played: " mode
  (gLifetime.gsGames .~)
  where
    mode = if batchMode then EGLtMins True else EGLifetime

-- | Prompt to edit a goalie's lifetime minutes played
editGoalieLtMinsPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Prompt
editGoalieLtMinsPrompt batchMode =
  editNum "Lifetime minutes played: " mode
  (gLifetime.gsMinsPlayed .~)
  where
    mode = if batchMode then EGLtGoals True else EGLifetime

-- | Prompt to edit a goalie's lifetime goals allowed
editGoalieLtGoalsPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Prompt
editGoalieLtGoalsPrompt batchMode =
  editNum "Lifetime goals allowed: " mode
  (gLifetime.gsGoalsAllowed .~)
  where
    mode = if batchMode then EGLtShutouts True else EGLifetime

-- | Prompt to edit a goalie's lifetime shutouts
editGoalieLtShutoutsPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Prompt
editGoalieLtShutoutsPrompt batchMode =
  editNum "Lifetime shutouts: " mode
  (gLifetime.gsShutouts .~)
  where
    mode = if batchMode then EGLtWins True else EGLifetime

-- | Prompt to edit a goalie's lifetime wins
editGoalieLtWinsPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Prompt
editGoalieLtWinsPrompt batchMode =
  editNum "Lifetime wins: " mode
  (gLifetime.gsWins .~)
  where
    mode = if batchMode then EGLtLosses True else EGLifetime

-- | Prompt to edit a goalie's lifetime losses
editGoalieLtLossesPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Prompt
editGoalieLtLossesPrompt batchMode =
  editNum "Lifetime losses: " mode
  (gLifetime.gsLosses .~)
  where
    mode = if batchMode then EGLtTies else EGLifetime

-- | Prompt to edit a goalie's lifetime ties
editGoalieLtTiesPrompt :: Prompt
editGoalieLtTiesPrompt = editNum "Lifetime ties: " EGLifetime
  (gLifetime.gsTies .~)

editNum
  :: String
  -> EditGoalieMode
  -> (Int -> Goalie -> Goalie)
  -> Prompt
editNum pStr mode f = numPromptWithFallback pStr
  (goto mode)
  (doEdit mode . f)

doEdit :: EditGoalieMode -> (Goalie -> Goalie) -> Action ()
doEdit mode f = do
  modify $ editSelectedGoalie f
  goto mode

goto :: EditGoalieMode -> Action ()
goto = modify . (progMode.editGoalieStateL.egsMode .~)
