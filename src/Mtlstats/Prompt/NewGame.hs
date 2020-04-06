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

{-# LANGUAGE LambdaCase #-}

module Mtlstats.Prompt.NewGame
  ( gameYearPrompt
  , gameDayPrompt
  , otherTeamPrompt
  , homeScorePrompt
  , awayScorePrompt
  , recordGoalPrompt
  , recordAssistPrompt
  , pMinPlayerPrompt
  , assignPMinsPrompt
  ) where

import Control.Monad (when)
import Control.Monad.Trans.State (gets, modify)
import Lens.Micro ((^.), (.~), (?~), (%~))

import Mtlstats.Actions.NewGame
import Mtlstats.Config
import Mtlstats.Format
import Mtlstats.Prompt
import Mtlstats.Types

-- | Prompts for the game year
gameYearPrompt :: Prompt
gameYearPrompt = numPrompt "Game year: " $
  modify . (progMode.gameStateL.gameYear ?~)

-- | Prompts for the day of the month the game took place
gameDayPrompt :: Prompt
gameDayPrompt = numPrompt "Day of month: " $
  modify . (progMode.gameStateL.gameDay ?~)

-- | Prompts for the other team name
otherTeamPrompt :: Prompt
otherTeamPrompt = ucStrPrompt "Other team: " $
  modify . (progMode.gameStateL.otherTeam .~)

-- | Prompts for the home score
homeScorePrompt :: Prompt
homeScorePrompt = numPrompt "Home score: " $
  modify . (progMode.gameStateL.homeScore ?~)

-- | Prompts for the away score
awayScorePrompt :: Prompt
awayScorePrompt = numPrompt "Away score: " $ \score -> modify
  $ overtimeCheck
  . (progMode.gameStateL.awayScore ?~ score)

-- | Prompts for the player who scored the goal
recordGoalPrompt
  :: Int
  -- ^ The game number
  -> Int
  -- ^ The goal number
  -> Prompt
recordGoalPrompt game goal = selectActivePlayerPrompt
  (  "*** GAME " ++ padNum 2 game ++ " ***\n"
  ++ "Who scored goal number " ++ show goal ++ "? "
  ) $ modify . (progMode.gameStateL.goalBy .~)

-- | Prompts for a player who assisted the goal
recordAssistPrompt
  :: Int
  -- ^ The game number
  -> Int
  -- ^ The goal nuber
  -> Int
  -- ^ The assist number
  -> Prompt
recordAssistPrompt game goal assist = selectActivePlayerPrompt
  (  "*** GAME " ++ padNum 2 game ++ " ***\n"
  ++ "Goal: " ++ show goal ++ "\n"
  ++ "Assist #" ++ show assist ++ ": "
  ) $ \case
    Nothing -> modify $ progMode.gameStateL.confirmGoalDataFlag .~ True
    Just n  -> do
      modify $ progMode.gameStateL.assistsBy %~ (++[n])
      nAssists <- length <$> gets (^.progMode.gameStateL.assistsBy)
      when (nAssists >= maxAssists) $
        modify $ progMode.gameStateL.confirmGoalDataFlag .~ True

-- | Prompts for the player to assign penalty minutes to
pMinPlayerPrompt :: Prompt
pMinPlayerPrompt = selectActivePlayerPrompt
  "Assign penalty minutes to: " $
  \case
    Nothing -> modify $ progMode.gameStateL.gamePMinsRecorded  .~ True
    Just n  -> modify $ progMode.gameStateL.gameSelectedPlayer ?~ n

-- | Prompts for the number of penalty mintues to assign to the player
assignPMinsPrompt :: Prompt
assignPMinsPrompt = numPrompt "Penalty minutes: " $
  modify . assignPMins
