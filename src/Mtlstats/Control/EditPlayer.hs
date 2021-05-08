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

module Mtlstats.Control.EditPlayer (editPlayerC) where

import Control.Monad.Trans.State (gets, modify)
import Data.Maybe (fromMaybe)
import Lens.Micro ((^.), (.~), (%~))
import qualified UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Handlers
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
  | otherwise =
    ( case eps^.epsMode of
      EPMenu         -> menuC
      EPNumber       -> numberC
      EPName         -> nameC
      EPPosition     -> positionC
      EPYtd          -> ytdC
      EPLifetime     -> lifetimeC
      EPDelete       -> deleteC
      EPYtdGoals   b -> ytdGoalsC   b
      EPYtdAssists b -> ytdAssistsC b
      EPYtdPMin      -> ytdPMinC
      EPLtGoals    b -> ltGoalsC    b
      EPLtAssists  b -> ltAssistsC  b
      EPLtPMin       -> ltPMinC
    ) $ eps^.epsCallback

selectPlayerC :: Controller
selectPlayerC = promptController playerToEditPrompt

menuC :: Action () -> Controller
menuC _ = menuControllerWith header editPlayerMenu

numberC :: Action () -> Controller
numberC = promptController . editPlayerNumPrompt

nameC :: Action () -> Controller
nameC = promptController . editPlayerNamePrompt

positionC :: Action () -> Controller
positionC = promptController . editPlayerPosPrompt

ytdC :: Action () -> Controller
ytdC _ = menuControllerWith header editPlayerYtdMenu

lifetimeC :: Action () -> Controller
lifetimeC _ = menuControllerWith header editPlayerLtMenu

deleteC :: Action () -> Controller
deleteC _ = Controller

  { drawController = \s -> do

    C.drawString $ let

      hdr = fromMaybe [] $ do
        pid    <- s^.progMode.editPlayerStateL.epsSelectedPlayer
        player <- nth pid $ s^.database.dbPlayers
        Just $ "Player: " ++ playerDetails player ++ "\n\n"

      in hdr ++ "Are you sure you want to delete this player? (Y/N)"

    return C.CursorInvisible

  , handleController = \e -> do

    case ynHandler e of

      Just True -> do
        gets (^.progMode.editPlayerStateL.epsSelectedPlayer) >>= mapM_
          (\pid -> modify $ database.dbPlayers %~ dropNth pid)
        modify edit

      Just False -> modify $ progMode.editPlayerStateL.epsMode .~ EPMenu
      Nothing    -> return ()

    return True

  }

ytdGoalsC :: Bool -> Action () -> Controller
ytdGoalsC batchMode callback = promptController $
  editPlayerYtdGoalsPrompt batchMode callback

ytdAssistsC :: Bool -> Action () -> Controller
ytdAssistsC batchMode callback = promptController $
  editPlayerYtdAssistsPrompt batchMode callback

ytdPMinC :: Action () -> Controller
ytdPMinC = promptController . editPlayerYtdPMinPrompt

ltGoalsC :: Bool -> Action () -> Controller
ltGoalsC batchMode callback = promptController $
  editPlayerLtGoalsPrompt batchMode callback

ltAssistsC :: Bool -> Action () -> Controller
ltAssistsC batchMode callback = promptController $
  editPlayerLtAssistsPrompt batchMode callback

ltPMinC :: Action () -> Controller
ltPMinC = promptController . editPlayerLtPMinPrompt

header :: ProgState -> C.Update ()
header s = C.drawString $ fromMaybe "" $ do
  pid    <- s^.progMode.editPlayerStateL.epsSelectedPlayer
  player <- nth pid $ s^.database.dbPlayers
  Just $ playerDetails player ++ "\n"
