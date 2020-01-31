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

module Mtlstats.Control (dispatch) where

import Control.Monad (join)
import Control.Monad.Trans.State (gets, modify)
import Data.Maybe (fromJust)
import Lens.Micro ((^.))
import qualified UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Control.CreatePlayer
import Mtlstats.Control.EditGoalie
import Mtlstats.Control.EditPlayer
import Mtlstats.Control.EditStandings
import Mtlstats.Control.NewGame
import Mtlstats.Control.TitleScreen
import Mtlstats.Handlers
import Mtlstats.Menu
import Mtlstats.Prompt
import Mtlstats.Types

-- | Reads the program state and returs the apropriate controller to
-- run
dispatch :: ProgState -> Controller
dispatch s = case s^.progMode of
  TitleScreen      -> titleScreenC
  MainMenu         -> mainMenuC
  NewSeason flag   -> newSeasonC flag
  NewGame gs       -> newGameC gs
  EditMenu         -> editMenuC
  CreatePlayer cps -> createPlayerC cps
  CreateGoalie cgs
    | null $ cgs^.cgsNumber -> getGoalieNumC
    | null $ cgs^.cgsName   -> getGoalieNameC
    | otherwise             -> confirmCreateGoalieC
  EditPlayer eps      -> editPlayerC eps
  EditGoalie egs      -> editGoalieC egs
  (EditStandings esm) -> editStandingsC esm

mainMenuC :: Controller
mainMenuC = Controller
  { drawController   = const $ drawMenu mainMenu
  , handleController = menuHandler mainMenu
  }

newSeasonC :: Bool -> Controller
newSeasonC False = promptController newSeasonPrompt
newSeasonC True  = menuController newSeasonMenu

editMenuC :: Controller
editMenuC = menuController editMenu

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
