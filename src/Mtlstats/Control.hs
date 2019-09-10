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

import Control.Monad (when)
import Control.Monad.Trans.State (modify)
import Data.Char (toUpper)
import Data.Maybe (fromJust)
import Lens.Micro ((^.), (.~))
import qualified UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Format
import Mtlstats.Handlers
import Mtlstats.Menu
import Mtlstats.Prompt
import Mtlstats.Report
import Mtlstats.Types

-- | Reads the program state and returs the apropriate controller to
-- run
dispatch :: ProgState -> Controller
dispatch s = case s^.progMode of
  MainMenu  -> mainMenuC
  NewSeason -> newSeasonC
  NewGame gs
    | null $ gs^.gameYear     -> gameYearC
    | null $ gs^.gameMonth    -> gameMonthC
    | null $ gs^.gameDay      -> gameDayC
    | null $ gs^.gameType     -> gameTypeC
    | null $ gs^.otherTeam    -> otherTeamC
    | null $ gs^.homeScore    -> homeScoreC
    | null $ gs^.awayScore    -> awayScoreC
    | null $ gs^.overtimeFlag -> overtimeFlagC
    | not $ gs^.dataVerified  -> verifyDataC
    | otherwise               -> reportC
  CreatePlayer cps
    | null $ cps^.cpsNumber   -> getPlayerNumC
    | null $ cps^.cpsName     -> getPlayerNameC
    | null $ cps^.cpsPosition -> getPlayerPosC
    | otherwise               -> undefined

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

reportC :: Controller
reportC = Controller
  { drawController = \s -> do
    (_, cols) <- C.windowSize
    C.drawString $ report (fromInteger $ pred cols) s
    return C.CursorInvisible
  , handleController = \e -> do
    when
      (case e of
        C.EventCharacter _  -> True
        C.EventSpecialKey _ -> True
        _                   -> False) $
      modify $ progMode .~ MainMenu
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
