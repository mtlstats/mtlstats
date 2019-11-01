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
numberC = undefined

nameC :: Controller
nameC = undefined

positionC :: Controller
positionC = undefined

ytdGoalsC :: Controller
ytdGoalsC = undefined

ytdAssistsC :: Controller
ytdAssistsC = undefined

ytdPMinC :: Controller
ytdPMinC = undefined

ltGoalsC :: Controller
ltGoalsC = undefined

ltAssistsC :: Controller
ltAssistsC = undefined

ltPMinC :: Controller
ltPMinC = undefined
