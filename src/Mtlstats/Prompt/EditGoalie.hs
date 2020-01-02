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
  , editGoalieLtMinsPrompt
  , editGoalieLtGoalsPrompt
  , editGoalieLtWinsPrompt
  , editGoalieLtLossesPrompt
  , editGoalieLtTiesPrompt
  ) where

import Control.Monad.Extra (whenJustM)
import Control.Monad.Trans.State (gets, modify)
import Lens.Micro ((^.), (.~), (%~))

import Mtlstats.Prompt
import Mtlstats.Types
import Mtlstats.Util

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
  else editGoalie EGMenu $ gName .~ name

-- | Prompt to edit a goalie's YTD games played
editGoalieYtdGamesPrompt :: Prompt
editGoalieYtdGamesPrompt = editNum "Year-to-date games played: " EGYtd
  (gYtd.gsGames .~)

-- | Prompt to edit a goalie's YTD minutes played
editGoalieYtdMinsPrompt :: Prompt
editGoalieYtdMinsPrompt = editNum "Year-to-date minutes played: " EGYtd
  (gYtd.gsMinsPlayed .~)

-- | Prompt to edit a goalie's YTD goales allowed
editGoalieYtdGoalsPrompt :: Prompt
editGoalieYtdGoalsPrompt = editNum "Year-to-date goals allowed: " EGYtd
  (gYtd.gsGoalsAllowed .~)

-- | Prompt to edit a goalie's YTD wins
editGoalieYtdWinsPrompt :: Prompt
editGoalieYtdWinsPrompt = editNum "Year-to-date wins: " EGYtd
  (gYtd.gsWins .~)

-- | Prompt to edit a goalie's YTD losses
editGoalieYtdLossesPrompt :: Prompt
editGoalieYtdLossesPrompt = editNum "Year-to-date losses: " EGYtd
  (gYtd.gsLosses .~)

-- | Prompt to edit a goalie's YTD ties
editGoalieYtdTiesPrompt :: Prompt
editGoalieYtdTiesPrompt = editNum "Year-to-date ties: " EGYtd
  (gYtd.gsTies .~)

-- | Prompt to edit a goalie's lifetime games played
editGoalieLtGamesPrompt :: Prompt
editGoalieLtGamesPrompt = editNum "Lifetime games played: " EGLifetime
  (gLifetime.gsGames .~)

-- | Prompt to edit a goalie's lifetime minutes played
editGoalieLtMinsPrompt :: Prompt
editGoalieLtMinsPrompt = editNum "Lifetime minutes played: " EGLifetime
  (gLifetime.gsMinsPlayed .~)

-- | Prompt to edit a goalie's lifetime goals allowed
editGoalieLtGoalsPrompt :: Prompt
editGoalieLtGoalsPrompt = editNum "Lifetime goals allowed: " EGLifetime
  (gLifetime.gsGoalsAllowed .~)

-- | Prompt to edit a goalie's lifetime wins
editGoalieLtWinsPrompt :: Prompt
editGoalieLtWinsPrompt = editNum "Lifetime wins: " EGLifetime
  (gLifetime.gsWins .~)

-- | Prompt to edit a goalie's lifetime losses
editGoalieLtLossesPrompt :: Prompt
editGoalieLtLossesPrompt = editNum "Lifetime losses: " EGLifetime
  (gLifetime.gsLosses .~)

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
  (editGoalie mode . f)

editGoalie :: EditGoalieMode -> (Goalie -> Goalie) -> Action ()
editGoalie mode f =
  whenJustM (gets (^.progMode.editGoalieStateL.egsSelectedGoalie)) $ \gid -> do
    modify $ database.dbGoalies %~ modifyNth gid f
    goto mode

goto :: EditGoalieMode -> Action ()
goto = modify . (progMode.editGoalieStateL.egsMode .~)
