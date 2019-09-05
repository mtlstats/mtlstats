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
import Lens.Micro ((^.), (.~))
import qualified UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Format
import Mtlstats.Menu
import Mtlstats.Prompt
import Mtlstats.Report
import Mtlstats.Types

-- | Reads the program state and returs the apropriate controller to
-- run
dispatch :: ProgState -> Controller
dispatch s = case s^.progMode of

  MainMenu -> Controller
    { drawController   = const $ drawMenu mainMenu
    , handleController = menuHandler mainMenu
    }

  NewSeason -> Controller
    { drawController   = const $ drawMenu newSeasonMenu
    , handleController = \e -> do
      menuHandler newSeasonMenu e
      return True
    }

  NewGame gs

    | null $ gs^.gameYear -> Controller
      { drawController = \s -> do
        header s
        drawPrompt gameYearPrompt s
      , handleController = \e -> do
        promptHandler gameYearPrompt e
        return True
      }

    | null $ gs^.gameMonth -> Controller
      { drawController = \s -> do
        header s
        drawMenu gameMonthMenu
      , handleController = \e -> do
        menuHandler gameMonthMenu e
        return True
      }

    | null $ gs^.gameDay -> Controller
      { drawController = \s -> do
        header s
        drawPrompt gameDayPrompt s
      , handleController = \e -> do
        promptHandler gameDayPrompt e
        modify validateGameDate
        return True
      }

    | null $ gs^.gameType -> Controller
      { drawController = \s -> do
        header s
        drawMenu gameTypeMenu
      , handleController = \e -> do
        menuHandler gameTypeMenu e
        return True
      }

    | null $ gs^.otherTeam -> Controller
      { drawController = \s -> do
        header s
        drawPrompt otherTeamPrompt s
      , handleController = \e -> do
        promptHandler otherTeamPrompt e
        return True
      }

    | null $ gs^.homeScore -> Controller
      { drawController = \s -> do
        header s
        drawPrompt homeScorePrompt s
      , handleController = \e -> do
        promptHandler homeScorePrompt e
        return True
      }

    | null $ gs^.awayScore -> Controller
      { drawController = \s -> do
        header s
        drawPrompt awayScorePrompt s
      , handleController = \e -> do
        promptHandler awayScorePrompt e
        modify overtimeCheck
        modify updateGameStats
        return True
      }

    | null $ gs^.overtimeFlag -> Controller
      { drawController = \s -> do
        header s
        C.drawString "Did the game go into overtime?  (Y/N)"
        return C.CursorInvisible
      , handleController = \e -> do
        overtimePrompt e
        modify updateGameStats
        return True
      }

    | otherwise -> Controller
      { drawController = \s -> do
        C.drawString $ report 72 s
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

overtimePrompt :: C.Event -> Action ()
overtimePrompt (C.EventCharacter c) = modify $
  progMode.gameStateL.overtimeFlag .~ case toUpper c of
    'Y' -> Just True
    'N' -> Just False
    _   -> Nothing
overtimePrompt _ = return ()
