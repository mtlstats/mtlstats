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

import Control.Monad (join)
import Control.Monad.Trans.State (gets, modify)
import Data.Maybe (fromJust)
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import qualified UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Control.EditPlayer
import Mtlstats.Control.NewGame
import Mtlstats.Handlers
import Mtlstats.Menu
import Mtlstats.Prompt
import Mtlstats.Types

-- | Reads the program state and returs the apropriate controller to
-- run
dispatch :: ProgState -> Controller
dispatch s = case s^.progMode of
  MainMenu  -> mainMenuC
  NewSeason -> newSeasonC
  NewGame _ -> newGameC s
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
