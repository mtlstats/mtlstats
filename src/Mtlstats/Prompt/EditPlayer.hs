{- |

mtlstats
Copyright (C) 1984, 1985, 2019, 2020, 2021 Rhéal Lamothe
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

import Control.Monad.Trans.State (modify)
import Lens.Micro ((.~))

import Mtlstats.Actions
import Mtlstats.Prompt
import Mtlstats.Types

-- | Prompt to edit a player's number
editPlayerNumPrompt
  :: Action ()
  -- ^ The action to be performed upon completion
  -> Prompt
editPlayerNumPrompt = editNum "Player number: " EPMenu
  (pNumber .~)

-- | Prompt to edit a player's name
editPlayerNamePrompt
  :: Action ()
  -- ^ The action to be performed upon completion
  -> Prompt
editPlayerNamePrompt callback = namePrompt "Player name: " $ \name -> do
  if null name
    then goto EPMenu
    else doEdit EPMenu $ pName .~ name
  callback

-- | Prompt to edit a player's position
editPlayerPosPrompt
  :: Action ()
  -- ^ The action to be performed upon completion
  -> Prompt
editPlayerPosPrompt callback = selectPositionPrompt "Player position: " $ \pos -> do
  if null pos
    then goto EPMenu
    else doEdit EPMenu $ pPosition .~ pos
  callback

-- | Prompt to edit a player's year-to-date goals
editPlayerYtdGoalsPrompt
  :: Bool
  -- ^ Indicates wheter or not we're editing in batch mode
  -> Action ()
  -- ^ The action to be performed upon completion
  -> Prompt
editPlayerYtdGoalsPrompt batchMode callback = editNum "Year-to-date goals: " mode
  (pYtd.psGoals .~) callback'
  where
    (mode, callback') = if batchMode
      then (EPYtdAssists True, return ())
      else (EPYtd, callback)

-- | Prompt to edit a player's year-to-date assists
editPlayerYtdAssistsPrompt
  :: Bool
  -- ^ Indicates wheter or not we're editing in batch mode
  -> Action ()
  -- ^ The action to be performed upon completion
  -> Prompt
editPlayerYtdAssistsPrompt batchMode callback = editNum "Year-to-date assists: " mode
  (pYtd.psAssists .~) callback'
  where
    (mode, callback') = if batchMode
      then (EPYtdPMin, return ())
      else (EPYtd, callback)

-- | Prompt to edit a player's year-to-date penalty minutes
editPlayerYtdPMinPrompt
  :: Action ()
  -- ^ The action to be performed upon completion
  -> Prompt
editPlayerYtdPMinPrompt = editNum "Year-to-date penalty minutes: " EPYtd
  (pYtd.psPMin .~)

-- | Prompt to edit a player's lifetime goals
editPlayerLtGoalsPrompt
  :: Bool
  -- ^ Indicates wheter or not we're editing in batch mode
  -> Action ()
  -- ^ The action to be performed upon completion
  -> Prompt
editPlayerLtGoalsPrompt batchMode callback = editNum "Lifetime goals: " mode
  (pLifetime.psGoals .~) callback'
  where
    (mode, callback') = if batchMode
      then (EPLtAssists True, return ())
      else (EPLifetime, callback)

-- | Prompt to edit a player's lifetime assists
editPlayerLtAssistsPrompt
  :: Bool
  -- ^ Indicates wheter or not we're editing in batch mode
  -> Action ()
  -- ^ The action to be performed upon completion
  -> Prompt
editPlayerLtAssistsPrompt batchMode callback = editNum "Lifetime assists: " mode
  (pLifetime.psAssists .~) callback'
  where
    (mode, callback') = if batchMode
      then (EPLtPMin, return ())
      else (EPLifetime, callback)

-- | Prompt to edit a player's lifetime penalty minutes
editPlayerLtPMinPrompt
  :: Action ()
  -- ^ The action to be performed upon completion
  -> Prompt
editPlayerLtPMinPrompt = editNum "Lifetime penalty minutes: " EPLifetime
  (pLifetime.psPMin .~)

editNum
  :: String
  -> EditPlayerMode
  -> (Int -> Player -> Player)
  -> Action ()
  -> Prompt
editNum pStr mode f callback = numPromptWithFallback pStr
  (goto mode >> callback)
  (\num -> do
    doEdit mode $ f num
    callback)

doEdit :: EditPlayerMode -> (Player -> Player) -> Action ()
doEdit mode f = do
  modify $ editSelectedPlayer f
  goto mode

goto :: EditPlayerMode -> Action ()
goto = modify . (progMode.editPlayerStateL.epsMode .~)
