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

module Mtlstats.Control.CreatePlayer (createPlayerC) where

import Control.Monad (join)
import Control.Monad.Trans.State (gets, modify)
import Data.Maybe (fromJust)
import Lens.Micro ((^.), (.~), (?~), (%~), to)
import qualified UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Handlers
import Mtlstats.Prompt
import Mtlstats.Types

-- | Handles player creation
createPlayerC :: CreatePlayerState -> Controller
createPlayerC cps
  | null $ cps^.cpsNumber   = getPlayerNumC
  | null $ cps^.cpsName     = getPlayerNameC
  | null $ cps^.cpsPosition = getPlayerPosC
  | otherwise               = confirmCreatePlayerC

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
        pid <- gets (^.database.dbPlayers.to length)
        cb  <- gets (^.progMode.createPlayerStateL.cpsSuccessCallback)
        modify
          $ (progMode.editPlayerStateL
            %~ (epsSelectedPlayer ?~ pid)
            .  (epsMode .~ EPLtGoals True)
            .  (epsCallback .~ cb))
          . addPlayer
      Just False ->
        join $ gets (^.progMode.createPlayerStateL.cpsFailureCallback)
      Nothing -> return ()
    return True
  }
