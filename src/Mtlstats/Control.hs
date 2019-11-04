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

module Mtlstats.Control (dispatch) where

import Control.Monad (join, when)
import Control.Monad.Extra (ifM)
import Control.Monad.Trans.State (gets, modify)
import Data.Char (toUpper)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Lens.Micro ((^.), (.~), (%~))
import Lens.Micro.Extras (view)
import qualified UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Control.EditPlayer
import Mtlstats.Control.GoalieInput
import Mtlstats.Format
import Mtlstats.Handlers
import Mtlstats.Menu
import Mtlstats.Prompt
import Mtlstats.Report
import Mtlstats.Types
import Mtlstats.Util

-- | Reads the program state and returs the apropriate controller to
-- run
dispatch :: ProgState -> Controller
dispatch s = case s^.progMode of
  MainMenu  -> mainMenuC
  NewSeason -> newSeasonC
  NewGame gs
    | null $ gs^.gameYear             -> gameYearC
    | null $ gs^.gameMonth            -> gameMonthC
    | null $ gs^.gameDay              -> gameDayC
    | null $ gs^.gameType             -> gameTypeC
    | null $ gs^.otherTeam            -> otherTeamC
    | null $ gs^.homeScore            -> homeScoreC
    | null $ gs^.awayScore            -> awayScoreC
    | null $ gs^.overtimeFlag         -> overtimeFlagC
    | not $ gs^.dataVerified          -> verifyDataC
    | fromJust (unaccountedPoints gs) -> goalInput gs
    | isJust $ gs^.gameSelectedPlayer -> getPMinsC
    | not $ gs^.gamePMinsRecorded     -> pMinPlayerC
    | not $ gs^.gameGoalieAssigned    -> goalieInput s
    | otherwise                       -> reportC
  CreatePlayer cps
    | null $ cps^.cpsNumber   -> getPlayerNumC
    | null $ cps^.cpsName     -> getPlayerNameC
    | null $ cps^.cpsPosition -> getPlayerPosC
    | otherwise               -> confirmCreatePlayerC
  CreateGoalie cgs
    | null $ cgs^.cgsNumber -> getGoalieNumC
    | null $ cgs^.cgsName   -> getGoalieNameC
    | otherwise             -> confirmCreateGoalieC
  EditPlayer eps -> editPlayerC eps

mainMenuC :: Controller
mainMenuC = Controller
  { drawController   = const $ drawMenu mainMenu
  , handleController = menuHandler mainMenu
  }

newSeasonC :: Controller
newSeasonC = Controller
  { drawController   = const $ drawMenu newSeasonMenu
  , handleController = \e -> do
    menuHandler newSeasonMenu e
    return True
  }

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

getPlayerNumC :: Controller
getPlayerNumC = Controller
  { drawController   = drawPrompt playerNumPrompt
  , handleController = \e -> do
    promptHandler playerNumPrompt e
    return True
  }

getPlayerNameC :: Controller
getPlayerNameC = Controller
  { drawController   = drawPrompt playerNamePrompt
  , handleController = \e -> do
    promptHandler playerNamePrompt e
    return True
  }

getPlayerPosC :: Controller
getPlayerPosC = Controller
  { drawController   = drawPrompt playerPosPrompt
  , handleController = \e -> do
    promptHandler playerPosPrompt e
    return True
  }

confirmCreatePlayerC :: Controller
confirmCreatePlayerC = Controller
  { drawController = \s -> do
    let cps = s^.progMode.createPlayerStateL
    C.drawString $ "  Player number: " ++ show (fromJust $ cps^.cpsNumber) ++ "\n"
    C.drawString $ "    Player name: " ++ cps^.cpsName ++ "\n"
    C.drawString $ "Player position: " ++ cps^.cpsPosition ++ "\n\n"
    C.drawString "Create player: are you sure?  (Y/N)"
    return C.CursorInvisible
  , handleController = \e -> do
    case ynHandler e of
      Just True -> do
        modify addPlayer
        join $ gets $ view $ progMode.createPlayerStateL.cpsSuccessCallback
      Just False ->
        join $ gets $ view $ progMode.createPlayerStateL.cpsFailureCallback
      Nothing -> return ()
    return True
  }

getGoalieNumC :: Controller
getGoalieNumC = Controller
  { drawController = drawPrompt goalieNumPrompt
  , handleController = \e -> do
    promptHandler goalieNumPrompt e
    return True
  }

getGoalieNameC :: Controller
getGoalieNameC = Controller
  { drawController = drawPrompt goalieNamePrompt
  , handleController = \e -> do
    promptHandler goalieNamePrompt e
    return True
  }

confirmCreateGoalieC :: Controller
confirmCreateGoalieC = Controller
  { drawController = \s -> do
    let cgs = s^.progMode.createGoalieStateL
    C.drawString $ unlines
      [ "Goalie number: " ++ show (fromJust $ cgs^.cgsNumber)
      , "  Goalie name: " ++ cgs^.cgsName
      , ""
      , "Create goalie: are you sure?  (Y/N)"
      ]
    return C.CursorInvisible
  , handleController = \e -> do
    case ynHandler e of
      Just True -> do
        modify addGoalie
        join $ gets (^.progMode.createGoalieStateL.cgsSuccessCallback)
      Just False ->
        join $ gets (^.progMode.createGoalieStateL.cgsFailureCallback)
      Nothing -> return ()
    return True
  }

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
