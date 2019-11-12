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

module Mtlstats.Actions.NewGame.GoalieInput
  ( finishGoalieEntry
  , recordGoalieStats
  , setGameGoalie
  ) where

import Control.Monad (void)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Lens.Micro ((^.), (&), (.~), (%~), (+~))

import Mtlstats.Config
import Mtlstats.Types
import Mtlstats.Util

-- | Attempts to finish game goalie entry
finishGoalieEntry :: ProgState -> ProgState
finishGoalieEntry s = s & progMode.gameStateL.gameGoaliesRecorded
  .~ not (null $ s^.progMode.gameStateL.gameGoalieStats)

-- | Records the goalie's game stats
recordGoalieStats :: ProgState -> ProgState
recordGoalieStats s = fromMaybe s $ do
  let gs = s^.progMode.gameStateL
  gid   <- gs^.gameSelectedGoalie
  mins  <- gs^.gameGoalieMinsPlayed
  goals <- gs^.gameGoalsAllowed
  void $ nth gid $ s^.database.dbGoalies

  let
    gameStats = M.findWithDefault newGoalieStats gid $ gs^.gameGoalieStats
    bumpVal   = if gameStats^.gsGames == 0
      then 1
      else 0

    bumpStats
      = (gsGames        +~ bumpVal)
      . (gsMinsPlayed   +~ mins)
      . (gsGoalsAllowed +~ goals)

    tryFinish = if mins >= gameLength
      then finishGoalieEntry
      else id

  Just $ s
    & progMode.gameStateL
      %~ (gameGoalieStats    %~ updateMap gid newGoalieStats bumpStats)
      .  (gameSelectedGoalie   .~ Nothing)
      .  (gameGoalieMinsPlayed .~ Nothing)
      .  (gameGoalsAllowed     .~ Nothing)
    & database.dbGoalies
      %~ modifyNth gid (\goalie -> goalie
         & gYtd      %~ bumpStats
         & gLifetime %~ bumpStats)
    & tryFinish

-- | Records the win, loss, or tie to a specific 'Goalie'
setGameGoalie
  :: Int
  -- ^ The goalie's index
  -> ProgState
  -> ProgState
setGameGoalie gid s = fromMaybe s $ do
  let gs = s^.progMode.gameStateL
  won  <- gameWon gs
  lost <- gameLost gs
  tied <- gs^.overtimeFlag
  let
    w = if won then 1 else 0
    l = if lost then 1 else 0
    t = if tied then 1 else 0

    updateStats
      = (gsWins   +~ w)
      . (gsLosses +~ l)
      . (gsTies   +~ t)

    updateGoalie
      = (gYtd      %~ updateStats)
      . (gLifetime %~ updateStats)

    updateGameState
      = (gameGoalieStats %~ updateMap gid newGoalieStats updateStats)
      . (gameGoalieAssigned .~ True)

  Just $ s
    & database.dbGoalies  %~ modifyNth gid updateGoalie
    & progMode.gameStateL %~ updateGameState
