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

module Mtlstats.Control.CreateGoalie (createGoalieC) where

import Control.Monad.Trans.State (gets, modify)
import Lens.Micro ((^.), (.~), (?~), (%~), to)
import qualified UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Format
import Mtlstats.Handlers
import Mtlstats.Prompt
import Mtlstats.Types

-- | Handles goalie creation
createGoalieC :: CreateGoalieState -> Controller
createGoalieC cgs
  | null $ cgs^.cgsNumber     = getGoalieNumC
  | null $ cgs^.cgsName       = getGoalieNameC
  | null $ cgs^.cgsRookieFlag = getRookieFlagC
  | otherwise                 = confirmCreateGoalieC

getGoalieNumC :: Controller
getGoalieNumC = promptController goalieNumPrompt

getGoalieNameC :: Controller
getGoalieNameC = promptController goalieNamePrompt

getRookieFlagC :: Controller
getRookieFlagC = Controller
  { drawController = const $ do
    C.drawString "Is this goalie a rookie? (Y/N)"
    return C.CursorInvisible
  , handleController = \e -> do
    modify $ progMode.createGoalieStateL.cgsRookieFlag .~ ynHandler e
    return True
  }

confirmCreateGoalieC :: Controller
confirmCreateGoalieC = Controller
  { drawController = \s -> do
    let cgs = s^.progMode.createGoalieStateL
    C.drawString $ unlines
      $  labelTable
         [ ( "Goalie number", maybe "?" show $ cgs^.cgsNumber     )
         , ( "Goalie name",   cgs^.cgsName                        )
         , ( "Rookie",        maybe "?" show $ cgs^.cgsRookieFlag )
         ]
      ++ [ ""
         , "Create goalie: are you sure?  (Y/N)"
         ]
    return C.CursorInvisible
  , handleController = \e -> do
    cgs <- gets (^.progMode.createGoalieStateL)
    let
      success = cgs^.cgsSuccessCallback
      failure = cgs^.cgsFailureCallback
    case ynHandler e of
      Just True -> do
        gid <- gets (^.database.dbGoalies.to length)
        let rookie = cgs^.cgsRookieFlag == Just True
        modify addGoalie
        if rookie
          then success
          else modify $ progMode.editGoalieStateL
            %~ (egsSelectedGoalie ?~ gid)
            .  (egsMode     .~ EGLtGames True)
            .  (egsCallback .~ success)
      Just False -> failure
      Nothing    -> return ()
    return True
  }
