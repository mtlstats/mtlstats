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
  , editPlayerLtPMinPrompt
  ) where

import Control.Monad.Extra (whenJustM)
import Control.Monad.Trans.State (gets, modify)
import Lens.Micro ((^.), (.~), (%~))

import Mtlstats.Prompt
import Mtlstats.Types
import Mtlstats.Util

-- | Prompt to edit a player's number
editPlayerNumPrompt :: Prompt
editPlayerNumPrompt = editNum "Player number: " EPMenu
  (pNumber .~)

-- | Prompt to edit a player's name
editPlayerNamePrompt :: Prompt
editPlayerNamePrompt = namePrompt "Player name: " $ \name ->
  if null name
  then goto EPMenu
  else editPlayer EPMenu $ pName .~ name

-- | Prompt to edit a player's position
editPlayerPosPrompt :: Prompt
editPlayerPosPrompt = ucStrPrompt "Player position: " $ \pos ->
  if null pos
  then goto EPMenu
  else editPlayer EPMenu $ pPosition .~ pos

-- | Prompt to edit a player's year-to-date goals
editPlayerYtdGoalsPrompt
  :: Bool
  -- ^ Indicates wheter or not we're editing in batch mode
  -> Prompt
editPlayerYtdGoalsPrompt batchMode = editNum "Year-to-date goals: " mode
  (pYtd.psGoals .~)
  where
    mode = if batchMode then EPYtdAssists True else EPYtd

-- | Prompt to edit a player's year-to-date assists
editPlayerYtdAssistsPrompt
  :: Bool
  -- ^ Indicates wheter or not we're editing in batch mode
  -> Prompt
editPlayerYtdAssistsPrompt batchMode = editNum "Year-to-date assists: " mode
  (pYtd.psAssists .~)
  where
    mode = if batchMode then EPYtdPMin else EPYtd

-- | Prompt to edit a player's year-to-date penalty minutes
editPlayerYtdPMinPrompt :: Prompt
editPlayerYtdPMinPrompt = editNum "Year-to-date penalty minutes: " EPYtd
  (pYtd.psPMin .~)

-- | Prompt to edit a player's lifetime goals
editPlayerLtGoalsPrompt
  :: Bool
  -- ^ Indicates wheter or not we're editing in batch mode
  -> Prompt
editPlayerLtGoalsPrompt batchMode = editNum "Lifetime goals: " mode
  (pLifetime.psGoals .~)
  where
    mode = if batchMode then EPLtAssists True else EPLifetime

-- | Prompt to edit a player's lifetime assists
editPlayerLtAssistsPrompt
  :: Bool
  -- ^ Indicates wheter or not we're editing in batch mode
  -> Prompt
editPlayerLtAssistsPrompt batchMode = editNum "Lifetime assists: " mode
  (pLifetime.psAssists .~)
  where
    mode = if batchMode then EPLtPMin else EPLifetime

-- | Prompt to edit a player's lifetime penalty minutes
editPlayerLtPMinPrompt :: Prompt
editPlayerLtPMinPrompt = editNum "Lifetime penalty minutes: " EPLifetime
  (pLifetime.psPMin .~)

editNum
  :: String
  -> EditPlayerMode
  -> (Int -> Player -> Player)
  -> Prompt
editNum pStr mode f = numPromptWithFallback pStr
  (goto mode)
  (editPlayer mode . f)

editPlayer :: EditPlayerMode -> (Player -> Player) -> Action ()
editPlayer mode f =
  whenJustM (gets (^.progMode.editPlayerStateL.epsSelectedPlayer)) $ \pid -> do
    modify $ database.dbPlayers %~ modifyNth pid f
    goto mode

goto :: EditPlayerMode -> Action ()
goto = modify . (progMode.editPlayerStateL.epsMode .~)
