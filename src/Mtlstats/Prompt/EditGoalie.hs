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
editGoalieNumberPrompt
  :: Action ()
  -- ^ Action to perform on completion
  -> Prompt
editGoalieNumberPrompt = editNum "Goalie number: " EGMenu
  (gNumber .~)

-- | Prompt to edit a goalie's name
editGoalieNamePrompt
  :: Action ()
  -- ^ Action to perform on completion
  -> Prompt
editGoalieNamePrompt cb = namePrompt "Goalie name: " $ \name -> do
  if null name
    then goto EGMenu
    else doEdit EGMenu $ gName .~ name
  cb

-- | Prompt to edit a goalie's YTD games played
editGoalieYtdGamesPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Action ()
  -- ^ Action to perform on completion
  -> Prompt
editGoalieYtdGamesPrompt batchMode cb =
  editNum "Year-to-date games played: " mode
  (gYtd.gsGames .~) cb'
  where
    (mode, cb') = if batchMode
      then (EGYtdMins True, return ())
      else (EGYtd, cb)

-- | Prompt to edit a goalie's YTD minutes played
editGoalieYtdMinsPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Action ()
  -- ^ Action to perform on completion
  -> Prompt
editGoalieYtdMinsPrompt batchMode cb =
  editNum "Year-to-date minutes played: " mode
  (gYtd.gsMinsPlayed .~) cb'
  where
    (mode, cb') = if batchMode
      then (EGYtdGoals True, return ())
      else (EGYtd, cb)

-- | Prompt to edit a goalie's YTD goales allowed
editGoalieYtdGoalsPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Action ()
  -- ^ Action to perform on completion
  -> Prompt
editGoalieYtdGoalsPrompt batchMode cb =
  editNum "Year-to-date goals allowed: " mode
  (gYtd.gsGoalsAllowed .~) cb'
  where
    (mode, cb') = if batchMode
      then (EGYtdShutouts True, return ())
      else (EGYtd, cb)

-- | Prompt to edit a goalie's YTD shutouts
editGoalieYtdShutoutsPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Action ()
  -- ^ Action to perform on completion
  -> Prompt
editGoalieYtdShutoutsPrompt batchMode cb =
  editNum "Year-to-date shutouts: " mode
  (gYtd.gsShutouts .~) cb'
  where
    (mode, cb') = if batchMode
      then (EGYtdWins True, return ())
      else (EGYtd, cb)

-- | Prompt to edit a goalie's YTD wins
editGoalieYtdWinsPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Action ()
  -- ^ Action to perform on completion
  -> Prompt
editGoalieYtdWinsPrompt batchMode cb =
  editNum "Year-to-date wins: " mode
  (gYtd.gsWins .~) cb'
  where
    (mode, cb') = if batchMode
      then (EGYtdLosses True, return ())
      else (EGYtd, cb)

-- | Prompt to edit a goalie's YTD losses
editGoalieYtdLossesPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Action ()
  -- ^ Action to perform on completion
  -> Prompt
editGoalieYtdLossesPrompt batchMode cb =
  editNum "Year-to-date losses: " mode
  (gYtd.gsLosses .~) cb'
  where
    (mode, cb') = if batchMode
      then (EGYtdTies, return ())
      else (EGYtd, cb)

-- | Prompt to edit a goalie's YTD ties
editGoalieYtdTiesPrompt
  :: Action ()
  -- ^ Action to perform on completion
  -> Prompt
editGoalieYtdTiesPrompt = editNum "Year-to-date ties: " EGYtd
  (gYtd.gsTies .~)

-- | Prompt to edit a goalie's lifetime games played
editGoalieLtGamesPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Action ()
  -- ^ Action to perform on completion
  -> Prompt
editGoalieLtGamesPrompt batchMode cb =
  editNum "Lifetime games played: " mode
  (gLifetime.gsGames .~) cb'
  where
    (mode, cb') = if batchMode
      then (EGLtMins True, return ())
      else (EGLifetime, cb)

-- | Prompt to edit a goalie's lifetime minutes played
editGoalieLtMinsPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Action ()
  -- ^ Action to perform on completion
  -> Prompt
editGoalieLtMinsPrompt batchMode cb =
  editNum "Lifetime minutes played: " mode
  (gLifetime.gsMinsPlayed .~) cb'
  where
    (mode, cb') = if batchMode
      then (EGLtGoals True, return ())
      else (EGLifetime, cb)

-- | Prompt to edit a goalie's lifetime goals allowed
editGoalieLtGoalsPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Action ()
  -- ^ Action to perform on completion
  -> Prompt
editGoalieLtGoalsPrompt batchMode cb =
  editNum "Lifetime goals allowed: " mode
  (gLifetime.gsGoalsAllowed .~) cb'
  where
    (mode, cb') = if batchMode
      then (EGLtShutouts True, return ())
      else (EGLifetime, cb)

-- | Prompt to edit a goalie's lifetime shutouts
editGoalieLtShutoutsPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Action ()
  -- ^ Action to perform on completion
  -> Prompt
editGoalieLtShutoutsPrompt batchMode cb =
  editNum "Lifetime shutouts: " mode
  (gLifetime.gsShutouts .~) cb'
  where
    (mode, cb') = if batchMode
      then (EGLtWins True, return ())
      else (EGLifetime, cb)

-- | Prompt to edit a goalie's lifetime wins
editGoalieLtWinsPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Action ()
  -- ^ Action to perform on completion
  -> Prompt
editGoalieLtWinsPrompt batchMode cb =
  editNum "Lifetime wins: " mode
  (gLifetime.gsWins .~) cb'
  where
    (mode, cb') = if batchMode
      then (EGLtLosses True, return ())
      else (EGLifetime, cb)

-- | Prompt to edit a goalie's lifetime losses
editGoalieLtLossesPrompt
  :: Bool
  -- ^ Indicates whether or not we're in batch mode
  -> Action ()
  -- ^ Action to perform on completion
  -> Prompt
editGoalieLtLossesPrompt batchMode cb =
  editNum "Lifetime losses: " mode
  (gLifetime.gsLosses .~) cb'
  where
    (mode, cb') = if batchMode
      then (EGLtTies, return ())
      else (EGLifetime, cb)

-- | Prompt to edit a goalie's lifetime ties
editGoalieLtTiesPrompt
  :: Action ()
  -- ^ Action to perform on completion
  -> Prompt
editGoalieLtTiesPrompt = editNum "Lifetime ties: " EGLifetime
  (gLifetime.gsTies .~)

editNum
  :: String
  -> EditGoalieMode
  -> (Int -> Goalie -> Goalie)
  -> Action ()
  -> Prompt
editNum pStr mode f cb = numPromptWithFallback pStr
  (goto mode >> cb)
  (\num -> do
    doEdit mode $ f num
    cb)

doEdit :: EditGoalieMode -> (Goalie -> Goalie) -> Action ()
doEdit mode f = do
  modify $ editSelectedGoalie f
  goto mode

goto :: EditGoalieMode -> Action ()
goto = modify . (progMode.editGoalieStateL.egsMode .~)
