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

module Mtlstats.Actions.GoalieInput
  ( finishGoalieEntry
  , recordGoalieStats
  ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Lens.Micro ((^.), (&), (.~), (%~), (+~))

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
  gid    <- gs^.gameSelectedGoalie
  goalie <- nth gid $ s^.database.dbGoalies
  mins   <- gs^.gameGoalieMinsPlayed
  goals  <- gs^.gameGoalsAllowed

  let
    gameStats = M.findWithDefault newGoalieStats gid $ gs^.gameGoalieStats
    bumpVal   = if gameStats^.gsGames == 0
      then 1
      else 0

    bumpStats gs = gs
      & gsGames        +~ bumpVal
      & gsMinsPlayed   +~ mins
      & gsGoalsAllowed +~ goals

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
