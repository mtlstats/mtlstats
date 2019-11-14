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

module Mtlstats.Actions.EditGoalie
  ( editGoalieNumber
  , editGoalieName
  , editGoalieYtdGames
  , editGoalieYtdMins
  ) where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Lens.Micro ((^.), (&), (.~), (%~))

import Mtlstats.Types
import Mtlstats.Util

-- | Edits a goalie's number
editGoalieNumber
  :: Int
  -- ^ New goalie number
  -> ProgState
  -> ProgState
editGoalieNumber num = editGoalie (gNumber .~ num) EGMenu

-- | Edits a goalie's name
editGoalieName
  :: String
  -- ^ The new name
  -> ProgState
  -> ProgState
editGoalieName name = editGoalie (gName .~ name) EGMenu

-- | Edits a goalie's YTD games
editGoalieYtdGames
  :: Int
  -- ^ The number of games played
  -> ProgState
  -> ProgState
editGoalieYtdGames games = editGoalie (gYtd.gsGames .~ games) EGYtd

-- | Edits a goalie's YTD minutes
editGoalieYtdMins
  :: Int
  -- ^ The number of minutes played
  -> ProgState
  -> ProgState
editGoalieYtdMins mins = editGoalie (gYtd.gsMinsPlayed .~ mins) EGYtd

editGoalie :: (Goalie -> Goalie) -> EditGoalieMode -> ProgState -> ProgState
editGoalie f mode s = fromMaybe s $ do
  gid <- s^.progMode.editGoalieStateL.egsSelectedGoalie
  void $ nth gid $ s^.database.dbGoalies
  Just $ s
    & database.dbGoalies %~ modifyNth gid f
    & progMode.editGoalieStateL.egsMode .~ mode
