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

module Mtlstats.Control.EditPlayer (editPlayerC) where

import Data.Maybe (fromMaybe)
import Lens.Micro ((^.))
import qualified UI.NCurses as C

import Mtlstats.Helpers.Player
import Mtlstats.Menu
import Mtlstats.Menu.EditPlayer
import Mtlstats.Prompt
import Mtlstats.Prompt.EditPlayer
import Mtlstats.Types
import Mtlstats.Util

-- | Dispatcher/controller for the player edit mode
editPlayerC :: EditPlayerState -> Controller
editPlayerC eps
  | null $ eps^.epsSelectedPlayer = selectPlayerC
  | otherwise = case eps^.epsMode of
    EPMenu         -> menuC
    EPNumber       -> numberC
    EPName         -> nameC
    EPPosition     -> positionC
    EPYtd          -> ytdC
    EPLifetime     -> lifetimeC
    EPYtdGoals   b -> ytdGoalsC   b
    EPYtdAssists b -> ytdAssistsC b
    EPYtdPMin      -> ytdPMinC
    EPLtGoals    b -> ltGoalsC    b
    EPLtAssists  b -> ltAssistsC  b
    EPLtPMin       -> ltPMinC

selectPlayerC :: Controller
selectPlayerC = promptController playerToEditPrompt

menuC :: Controller
menuC = menuControllerWith header editPlayerMenu

numberC :: Controller
numberC = promptController editPlayerNumPrompt

nameC :: Controller
nameC = promptController editPlayerNamePrompt

positionC :: Controller
positionC = promptController editPlayerPosPrompt

ytdC :: Controller
ytdC = menuControllerWith header editPlayerYtdMenu

lifetimeC :: Controller
lifetimeC = menuControllerWith header editPlayerLtMenu

ytdGoalsC :: Bool -> Controller
ytdGoalsC = promptController . editPlayerYtdGoalsPrompt

ytdAssistsC :: Bool -> Controller
ytdAssistsC = promptController . editPlayerYtdAssistsPrompt

ytdPMinC :: Controller
ytdPMinC = promptController editPlayerYtdPMinPrompt

ltGoalsC :: Bool -> Controller
ltGoalsC = promptController . editPlayerLtGoalsPrompt

ltAssistsC :: Bool -> Controller
ltAssistsC = promptController . editPlayerLtAssistsPrompt

ltPMinC :: Controller
ltPMinC = promptController editPlayerLtPMinPrompt

header :: ProgState -> C.Update ()
header s = C.drawString $ fromMaybe "" $ do
  pid    <- s^.progMode.editPlayerStateL.epsSelectedPlayer
  player <- nth pid $ s^.database.dbPlayers
  Just $ playerDetails player ++ "\n"
