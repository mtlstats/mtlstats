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

module Mtlstats.Control.NewGame (newGameC) where

import Control.Monad.Trans.State (gets, modify)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Lens.Micro ((^.), (.~))
import qualified UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Actions.NewGame
import Mtlstats.Control.NewGame.GoalieInput
import Mtlstats.Format
import Mtlstats.Handlers
import Mtlstats.Menu
import Mtlstats.Prompt
import Mtlstats.Prompt.NewGame
import Mtlstats.Report
import Mtlstats.Types
import Mtlstats.Util

-- | Dispatcher for a new game
newGameC :: ProgState -> Controller
newGameC s = let
  gs = s^.progMode.gameStateL
  in if null $ gs^.gameYear               then gameYearC
  else if null $ gs^.gameMonth            then gameMonthC
  else if null $ gs^.gameDay              then gameDayC
  else if null $ gs^.gameType             then gameTypeC
  else if null $ gs^.otherTeam            then otherTeamC
  else if null $ gs^.homeScore            then homeScoreC
  else if null $ gs^.awayScore            then awayScoreC
  else if null $ gs^.overtimeFlag         then overtimeFlagC
  else if not $ gs^.dataVerified          then verifyDataC
  else if fromJust (unaccountedPoints gs) then goalInput gs
  else if isJust $ gs^.gameSelectedPlayer then getPMinsC
  else if not $ gs^.gamePMinsRecorded     then pMinPlayerC
  else if not $ gs^.gameGoalieAssigned    then goalieInputC s
  else reportC

gameYearC :: Controller
gameYearC = Controller
  { drawController = \s -> do
    header s
    drawPrompt gameYearPrompt s
  , handleController = \e -> do
    promptHandler gameYearPrompt e
    return True
  }

gameMonthC :: Controller
gameMonthC = Controller
  { drawController = \s -> do
    header s
    drawMenu gameMonthMenu
  , handleController = \e -> do
    menuHandler gameMonthMenu e
    return True
  }

gameDayC :: Controller
gameDayC = Controller
  { drawController = \s -> do
    header s
    drawPrompt gameDayPrompt s
  , handleController = \e -> do
    promptHandler gameDayPrompt e
    modify validateGameDate
    return True
  }

gameTypeC :: Controller
gameTypeC = Controller
  { drawController = \s -> do
    header s
    drawMenu gameTypeMenu
  , handleController = \e -> do
    menuHandler gameTypeMenu e
    return True
  }

otherTeamC :: Controller
otherTeamC = Controller
  { drawController = \s -> do
    header s
    drawPrompt otherTeamPrompt s
  , handleController = \e -> do
    promptHandler otherTeamPrompt e
    return True
  }

homeScoreC :: Controller
homeScoreC = Controller
  { drawController = \s -> do
    header s
    drawPrompt homeScorePrompt s
  , handleController = \e -> do
    promptHandler homeScorePrompt e
    return True
  }

awayScoreC :: Controller
awayScoreC = Controller
  { drawController = \s -> do
    header s
    drawPrompt awayScorePrompt s
  , handleController = \e -> do
    promptHandler awayScorePrompt e
    modify overtimeCheck
    return True
  }

overtimeFlagC :: Controller
overtimeFlagC = Controller
  { drawController = \s -> do
    header s
    C.drawString "Did the game go into overtime?  (Y/N)"
    return C.CursorInvisible
  , handleController = \e -> do
    modify $ progMode.gameStateL.overtimeFlag .~ ynHandler e
    return True
  }

verifyDataC :: Controller
verifyDataC = Controller
  { drawController = \s -> do
    let gs = s^.progMode.gameStateL
    header s
    C.drawString "\n"
    C.drawString $ "      Date: " ++ gameDate gs ++ "\n"
    C.drawString $ " Game type: " ++ show (fromJust $ gs^.gameType) ++ "\n"
    C.drawString $ "Other team: " ++ gs^.otherTeam ++ "\n"
    C.drawString $ "Home score: " ++ show (fromJust $ gs^.homeScore) ++ "\n"
    C.drawString $ "Away score: " ++ show (fromJust $ gs^.awayScore) ++ "\n"
    C.drawString $ "  Overtime: " ++ show (fromJust $ gs^.overtimeFlag) ++ "\n\n"
    C.drawString "Is the above information correct?  (Y/N)"
    return C.CursorInvisible
  , handleController = \e -> do
    case ynHandler e of
      Just True  -> do
        modify $ progMode.gameStateL.dataVerified .~ True
        modify updateGameStats
      Just False -> modify $ progMode.gameStateL .~ newGameState
      Nothing    -> return ()
    return True
  }

goalInput :: GameState -> Controller
goalInput gs
  | null (gs^.goalBy            ) = recordGoalC
  | not (gs^.confirmGoalDataFlag) = recordAssistC
  | otherwise                     = confirmGoalDataC

recordGoalC :: Controller
recordGoalC = Controller
  { drawController = \s -> let
    (game, goal) = gameGoal s
    in drawPrompt (recordGoalPrompt game goal) s
  , handleController = \e -> do
    (game, goal) <- gets gameGoal
    promptHandler (recordGoalPrompt game goal) e
    return True
  }

recordAssistC :: Controller
recordAssistC = Controller
  { drawController = \s -> let
    (game, goal, assist) = gameGoalAssist s
    in drawPrompt (recordAssistPrompt game goal assist) s
  , handleController = \e -> do
    (game, goal, assist) <- gets gameGoalAssist
    promptHandler (recordAssistPrompt game goal assist) e
    return True
  }

confirmGoalDataC :: Controller
confirmGoalDataC = Controller
  { drawController = \s -> do
    let
      (game, goal) = gameGoal s
      gs           = s^.progMode.gameStateL
      players      = s^.database.dbPlayers
      msg          = unlines $
        [ "          Game: " ++ padNum 2 game
        , "          Goal: " ++ show goal
        , "Goal scored by: " ++
          playerSummary (fromJust $ gs^.goalBy >>= flip nth players)
        ] ++
        map
          (\pid -> "   Assisted by: " ++
            playerSummary (fromJust $ nth pid players))
          (gs^.assistsBy) ++
        [ ""
        , "Is the above information correct? (Y/N)"
        ]
    C.drawString msg
    return C.CursorInvisible
  , handleController = \e -> do
    case ynHandler e of
      Just True  -> modify recordGoalAssists
      Just False -> modify resetGoalData
      Nothing    -> return ()
    return True
  }

pMinPlayerC :: Controller
pMinPlayerC = Controller
  { drawController = \s -> do
    header s
    drawPrompt pMinPlayerPrompt s
  , handleController = \e -> do
    promptHandler pMinPlayerPrompt e
    return True
  }

getPMinsC :: Controller
getPMinsC = Controller
  { drawController = \s -> do
    header s
    C.drawString $ fromMaybe "" $ do
      pid    <- s^.progMode.gameStateL.gameSelectedPlayer
      player <- nth pid $ s^.database.dbPlayers
      Just $ playerSummary player ++ "\n"
    drawPrompt assignPMinsPrompt s
  , handleController = \e -> do
    promptHandler assignPMinsPrompt e
    return True
  }

reportC :: Controller
reportC = Controller
  { drawController = \s -> do
    (rows, cols) <- C.windowSize
    C.drawString $ unlines $ slice
      (s^.scrollOffset)
      (fromInteger $ pred rows)
      (report (fromInteger $ pred cols) s)
    return C.CursorInvisible
  , handleController = \e -> do
    case e of
      C.EventSpecialKey C.KeyUpArrow   -> modify scrollUp
      C.EventSpecialKey C.KeyDownArrow -> modify scrollDown
      C.EventSpecialKey C.KeyHome      -> modify $ scrollOffset .~ 0
      C.EventSpecialKey _              -> modify backHome
      C.EventCharacter _               -> modify backHome
      _                                -> return ()
    return True
  }

header :: ProgState -> C.Update ()
header s = C.drawString $
  "*** GAME " ++ padNum 2 (s^.database.dbGames) ++ " ***\n"

gameGoal :: ProgState -> (Int, Int)
gameGoal s =
  ( s^.database.dbGames
  , succ $ s^.progMode.gameStateL.pointsAccounted
  )

gameGoalAssist :: ProgState -> (Int, Int, Int)
gameGoalAssist s = let
  (game, goal) = gameGoal s
  assist       = succ $ length $ s^.progMode.gameStateL.assistsBy
  in (game, goal, assist)
