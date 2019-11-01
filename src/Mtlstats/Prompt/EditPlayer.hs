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

module Mtlstats.Prompt.EditPlayer
  ( editPlayerNumPrompt
  , editPlayerNamePrompt
  , editPlayerPosPrompt
  , editPlayerYtdGoalsPrompt
  , editPlayerYtdAssistsPrompt
  , editPlayerYtdPMinPrompt
  , editPlayerLtGoalsPrompt
  , editPlayerLtAssistsPrompt
  ) where

import Control.Monad.Extra (whenJustM)
import Control.Monad.Trans.State (gets, modify)
import Lens.Micro ((^.), (.~), (%~))

import Mtlstats.Prompt
import Mtlstats.Types
import Mtlstats.Util

-- | Prompt to edit a player's number
editPlayerNumPrompt :: Prompt
editPlayerNumPrompt = numPrompt "Player number: " $
  editPlayer . (pNumber .~)

-- | Prompt to edit a player's name
editPlayerNamePrompt :: Prompt
editPlayerNamePrompt = strPrompt "Player name: " $
  editPlayer . (pName .~)

-- | Prompt to edit a player's position
editPlayerPosPrompt :: Prompt
editPlayerPosPrompt = strPrompt "Player position: " $
  editPlayer . (pPosition .~)

-- | Prompt to edit a player's year-to-date goals
editPlayerYtdGoalsPrompt :: Prompt
editPlayerYtdGoalsPrompt = numPrompt "Year-to-date goals: " $
  editPlayer . (pYtd.psGoals .~)

-- | Prompt to edit a player's year-to-date assists
editPlayerYtdAssistsPrompt :: Prompt
editPlayerYtdAssistsPrompt = numPrompt "Year-to-date assists: " $
  editPlayer . (pYtd.psAssists .~)

-- | Prompt to edit a player's year-to-date penalty minutes
editPlayerYtdPMinPrompt :: Prompt
editPlayerYtdPMinPrompt = numPrompt "Year-to-date penalty minutes: " $
  editPlayer . (pYtd.psPMin .~)

-- | Prompt to edit a player's lifetime goals
editPlayerLtGoalsPrompt :: Prompt
editPlayerLtGoalsPrompt = numPrompt "Lifetime goals: " $
  editPlayer . (pLifetime.psGoals .~)

-- | Prompt to edit a player's lifetime assists
editPlayerLtAssistsPrompt :: Prompt
editPlayerLtAssistsPrompt = numPrompt "Lifetime assists: " $
  editPlayer . (pLifetime.psAssists .~)

editPlayer :: (Player -> Player) -> Action ()
editPlayer f =
  whenJustM (gets (^.progMode.editPlayerStateL.epsSelectedPlayer)) $ \pid ->
    modify
      $ (database.dbPlayers %~ modifyNth pid f)
      . (progMode.editPlayerStateL.epsMode .~ EPMenu)
