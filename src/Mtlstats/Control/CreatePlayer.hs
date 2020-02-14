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

import Control.Monad.Trans.State (gets, modify)
import Lens.Micro ((^.), (.~), (?~), (%~), to)
import qualified UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Format
import Mtlstats.Handlers
import Mtlstats.Prompt
import Mtlstats.Types

-- | Handles player creation
createPlayerC :: CreatePlayerState -> Controller
createPlayerC cps
  | null $ cps^.cpsNumber     = getPlayerNumC
  | null $ cps^.cpsName       = getPlayerNameC
  | null $ cps^.cpsPosition   = getPlayerPosC
  | null $ cps^.cpsRookieFlag = getRookieFlagC
  | null $ cps^.cpsActiveFlag = getActiveFlagC
  | otherwise                 = confirmCreatePlayerC

getPlayerNumC :: Controller
getPlayerNumC = promptController playerNumPrompt

getPlayerNameC :: Controller
getPlayerNameC = promptController playerNamePrompt

getPlayerPosC :: Controller
getPlayerPosC = promptController playerPosPrompt

getRookieFlagC :: Controller
getRookieFlagC = Controller
  { drawController = const $ do
    C.drawString "Is this player a rookie? (Y/N)"
    return C.CursorInvisible
  , handleController = \e -> do
    modify $ progMode.createPlayerStateL.cpsRookieFlag .~ ynHandler e
    return True
  }

getActiveFlagC :: Controller
getActiveFlagC = Controller
  { drawController = const $ do
    C.drawString "Is the player active? (Y/N)"
    return C.CursorInvisible
  , handleController = \e -> do
    modify $ progMode.createPlayerStateL.cpsActiveFlag .~ ynHandler e
    return True
  }

confirmCreatePlayerC :: Controller
confirmCreatePlayerC = Controller
  { drawController = \s -> do
    let cps = s^.progMode.createPlayerStateL
    C.drawString $ unlines
      $  labelTable
         [ ( "Player number",   maybe "?" show $ cps^.cpsNumber     )
         , ( "Player name",     cps^.cpsName                        )
         , ( "Player position", cps^.cpsPosition                    )
         , ( "Rookie",          maybe "?" show $ cps^.cpsRookieFlag )
         ]
      ++ [ ""
         , "Create player: are you sure?  (Y/N)"
         ]
    return C.CursorInvisible
  , handleController = \e -> do
    cps <- gets (^.progMode.createPlayerStateL)
    let
      success = cps^.cpsSuccessCallback
      failure = cps^.cpsFailureCallback
    case ynHandler e of
      Just True -> do
        pid <- gets (^.database.dbPlayers.to length)
        let rookie = cps^.cpsRookieFlag == Just True
        modify addPlayer
        if rookie
          then success
          else modify $ progMode.editPlayerStateL
            %~ (epsSelectedPlayer ?~ pid)
            .  (epsMode           .~ EPLtGoals True)
            .  (epsCallback       .~ success)
      Just False -> failure
      Nothing -> return ()
    return True
  }
