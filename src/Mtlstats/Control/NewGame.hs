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

module Mtlstats.Control.NewGame (newGameC) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (get, gets, modify)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Lens.Micro ((^.), (.~))
import qualified UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Actions.NewGame
import Mtlstats.Config
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
newGameC :: GameState -> Controller
newGameC gs
  | null $ gs^.gameYear             = gameYearC
  | null $ gs^.gameMonth            = gameMonthC
  | null $ gs^.gameDay              = gameDayC
  | null $ gs^.gameType             = gameTypeC
  | null $ gs^.otherTeam            = otherTeamC
  | null $ gs^.homeScore            = homeScoreC
  | null $ gs^.awayScore            = awayScoreC
  | null $ gs^.overtimeFlag         = overtimeFlagC
  | not $ gs^.dataVerified          = verifyDataC
  | fromJust (unaccountedPoints gs) = goalInput gs
  | isJust $ gs^.gameSelectedPlayer = getPMinsC
  | not $ gs^.gamePMinsRecorded     = pMinPlayerC
  | not $ gs^.gameGoalieAssigned    = goalieInputC gs
  | otherwise                       = reportC

gameYearC :: Controller
gameYearC = promptControllerWith header gameYearPrompt

gameMonthC :: Controller
gameMonthC = promptControllerWith monthHeader gameMonthPrompt

gameDayC :: Controller
gameDayC = promptControllerWith header gameDayPrompt

gameTypeC :: Controller
gameTypeC = menuControllerWith header gameTypeMenu

otherTeamC :: Controller
otherTeamC = promptControllerWith header otherTeamPrompt

homeScoreC :: Controller
homeScoreC = promptControllerWith header homeScorePrompt

awayScoreC :: Controller
awayScoreC = promptControllerWith header awayScorePrompt

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
    C.drawString $ unlines $ labelTable
      [ ( "Date",       gameDate gs                        )
      , ( "Game type",  show $ fromJust $ gs^.gameType     )
      , ( "Other team", gs^.otherTeam                      )
      , ( "Home score", show $ fromJust $ gs^.homeScore    )
      , ( "Away score", show $ fromJust $ gs^.awayScore    )
      , ( "Overtime",   show $ fromJust $ gs^.overtimeFlag )
      ]
    C.drawString "\nIs the above information correct?  (Y/N)"
    return C.CursorInvisible
  , handleController = \e -> do
    case ynHandler e of
      Just True  -> modify
        $ (progMode.gameStateL.dataVerified .~ True)
        . updateGameStats
        . awardShutouts
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
      (displayReport (fromInteger $ pred cols) s)
    return C.CursorInvisible
  , handleController = \e -> do
    case e of
      C.EventSpecialKey C.KeyUpArrow   -> modify scrollUp
      C.EventSpecialKey C.KeyDownArrow -> modify scrollDown
      C.EventSpecialKey C.KeyHome      -> modify $ scrollOffset .~ 0

      C.EventCharacter '\n' -> do
        get >>= liftIO . writeFile reportFilename . exportReport reportCols
        modify backHome

      _ -> return ()
    return True
  }

header :: ProgState -> C.Update ()
header s = C.drawString $
  "*** GAME " ++ padNum 2 (s^.database.dbGames) ++ " ***\n"

monthHeader :: ProgState -> C.Update ()
monthHeader s = do
  (_, cols) <- C.windowSize
  header s

  let
    table = labelTable $ zip (map show ([1..] :: [Int]))
      [ "JANUARY"
      , "FEBRUARY"
      , "MARCH"
      , "APRIL"
      , "MAY"
      , "JUNE"
      , "JULY"
      , "AUGUST"
      , "SEPTEMBER"
      , "OCTOBER"
      , "NOVEMBER"
      , "DECEMBER"
      ]

  C.drawString $ unlines $
    map (centre $ fromIntegral $ pred cols) $
      ["MONTH:", ""] ++ table ++ [""]

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
