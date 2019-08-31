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

{-# LANGUAGE LambdaCase #-}

module Mtlstats.Events (handleEvent) where

import Control.Monad.Trans.State (gets, modify)
import Data.Char (toUpper)
import Lens.Micro ((^.), (.~))
import Lens.Micro.Extras (view)
import qualified UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Menu
import Mtlstats.Prompt
import Mtlstats.Types

-- | Event handler
handleEvent
  :: C.Event
  -- ^ The event being handled
  -> Action Bool
handleEvent e = gets (view progMode) >>= \case
  MainMenu  -> menuHandler mainMenu e
  NewSeason -> menuHandler newSeasonMenu e >> return True
  NewGame gs
    | null $ gs^.gameType -> do
      menuHandler gameTypeMenu e
      return True
    | null $ gs^.otherTeam -> do
      promptHandler otherTeamPrompt e
      return True
    | null $ gs^.homeScore -> do
      promptHandler homeScorePrompt e
      return True
    | null $ gs^.awayScore -> do
      promptHandler awayScorePrompt e
      modify overtimeCheck
      return True
    | null $ gs^.overtimeFlag -> do
      overtimePrompt e
        >>= modify . (progMode.gameStateL.overtimeFlag .~)
      modify updateGameStats
      return True
    | otherwise -> undefined

overtimePrompt :: C.Event -> Action (Maybe Bool)
overtimePrompt (C.EventCharacter c) = case toUpper c of
  'Y' -> return (Just True)
  'N' -> return (Just False)
  _   -> return Nothing
