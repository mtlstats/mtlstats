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

module Mtlstats.Control.EditPlayer (editPlayerC) where

import Data.Maybe (fromMaybe)
import Lens.Micro ((^.))
import qualified UI.NCurses as C

import Mtlstats.Menu
import Mtlstats.Prompt
import Mtlstats.Prompt.EditPlayer
import Mtlstats.Types
import Mtlstats.Util

-- | Dispatcher/controller for the player edit mode
editPlayerC :: EditPlayerState -> Controller
editPlayerC eps
  | null $ eps^.epsSelectedPlayer = selectPlayerC
  | otherwise = case eps^.epsMode of
    EPMenu       -> menuC
    EPNumber     -> numberC
    EPName       -> nameC
    EPPosition   -> positionC
    EPYtdGoals   -> ytdGoalsC
    EPYtdAssists -> ytdAssistsC
    EPYtdPMin    -> ytdPMinC
    EPLtGoals    -> ltGoalsC
    EPLtAssists  -> ltAssistsC
    EPLtPMin     -> ltPMinC

selectPlayerC :: Controller
selectPlayerC = Controller
  { drawController   = drawPrompt playerToEditPrompt
  , handleController = \e -> do
    promptHandler playerToEditPrompt e
    return True
  }

menuC :: Controller
menuC = Controller
  { drawController = \s -> do
    let
      header = fromMaybe "" $ do
        pid <- s^.progMode.editPlayerStateL.epsSelectedPlayer
        p   <- nth pid $ s^.database.dbPlayers
        Just $ playerDetails p ++ "\n"
    C.drawString header
    drawMenu editPlayerMenu
  , handleController = \e -> do
    menuHandler editPlayerMenu e
    return True
  }

numberC :: Controller
numberC = Controller
  { drawController = drawPrompt editPlayerNumPrompt
  , handleController = \e -> do
    promptHandler editPlayerNumPrompt e
    return True
  }

nameC :: Controller
nameC = Controller
  { drawController   = drawPrompt editPlayerNamePrompt
  , handleController = \e -> do
    promptHandler editPlayerNamePrompt e
    return True
  }

positionC :: Controller
positionC = Controller
  { drawController   = drawPrompt editPlayerPosPrompt
  , handleController = \e -> do
    promptHandler editPlayerPosPrompt e
    return True
  }

ytdGoalsC :: Controller
ytdGoalsC = Controller
  { drawController   = drawPrompt editPlayerYtdGoalsPrompt
  , handleController = \e -> do
    promptHandler editPlayerYtdGoalsPrompt e
    return True
  }

ytdAssistsC :: Controller
ytdAssistsC = Controller
  { drawController   = drawPrompt editPlayerYtdAssistsPrompt
  , handleController = \e -> do
    promptHandler editPlayerYtdAssistsPrompt e
    return True
  }

ytdPMinC :: Controller
ytdPMinC = Controller
  { drawController   = drawPrompt editPlayerYtdPMinPrompt
  , handleController = \e -> do
    promptHandler editPlayerYtdPMinPrompt e
    return True
  }

ltGoalsC :: Controller
ltGoalsC = Controller
  { drawController   = drawPrompt editPlayerLtGoalsPrompt
  , handleController = \e -> do
    promptHandler editPlayerLtGoalsPrompt e
    return True
  }

ltAssistsC :: Controller
ltAssistsC = Controller
  { drawController   = drawPrompt editPlayerLtAssistsPrompt
  , handleController = \e -> do
    promptHandler editPlayerLtAssistsPrompt e
    return True
  }

ltPMinC :: Controller
ltPMinC = Controller
  { drawController   = drawPrompt editPlayerLtPMinPrompt
  , handleController = \e -> do
    promptHandler editPlayerLtPMinPrompt e
    return True
  }
