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

{-# LANGUAGE LambdaCase #-}

module Mtlstats.Control.EditGoalie (editGoalieC) where

import Control.Monad.Trans.State (gets, modify)
import Data.Maybe (fromMaybe)
import Lens.Micro ((^.), (%~))
import UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Handlers
import Mtlstats.Helpers.Goalie
import Mtlstats.Menu
import Mtlstats.Menu.EditGoalie
import Mtlstats.Prompt
import Mtlstats.Prompt.EditGoalie
import Mtlstats.Types
import Mtlstats.Util

-- | Controller/dispatcher for editing a 'Goalie'
editGoalieC :: EditGoalieState -> Controller
editGoalieC egs
  | null $ egs^.egsSelectedGoalie = selectC
  | otherwise = editC (egs^.egsCallback) (egs^.egsMode)

selectC :: Controller
selectC = promptController goalieToEditPrompt

editC :: Action () -> EditGoalieMode -> Controller
editC cb =
  ( \case
    EGMenu          -> menuC
    EGNumber        -> numberC
    EGName          -> nameC
    EGYtd           -> ytdMenuC
    EGLifetime      -> lifetimeMenuC
    EGDelete        -> deleteC
    EGYtdGames b    -> ytdGamesC b
    EGYtdMins b     -> ytdMinsC b
    EGYtdGoals b    -> ytdGoalsC b
    EGYtdShutouts b -> ytdShutoutsC b
    EGYtdWins b     -> ytdWinsC b
    EGYtdLosses b   -> ytdLossesC b
    EGYtdTies       -> ytdTiesC
    EGLtGames b     -> ltGamesC b
    EGLtMins b      -> ltMinsC b
    EGLtGoals b     -> ltGoalsC b
    EGLtShutouts b  -> ltShutoutsC b
    EGLtWins b      -> ltWinsC b
    EGLtLosses b    -> ltLossesC b
    EGLtTies        -> ltTiesC
  ) <*> return cb

menuC :: Action () -> Controller
menuC _ = menuControllerWith header editGoalieMenu

numberC :: Action () -> Controller
numberC = promptController . editGoalieNumberPrompt

nameC :: Action () -> Controller
nameC = promptController . editGoalieNamePrompt

ytdMenuC :: Action () -> Controller
ytdMenuC _ = menuControllerWith header editGoalieYtdMenu

lifetimeMenuC :: Action () -> Controller
lifetimeMenuC _ = menuControllerWith header editGoalieLtMenu

deleteC :: Action () -> Controller
deleteC _ = Controller

  { drawController = \s -> do

    C.drawString $ let

      hdr = fromMaybe [] $ do
        gid    <- s^.progMode.editGoalieStateL.egsSelectedGoalie
        goalie <- nth gid $ s^.database.dbGoalies
        Just $ "Goalie: " ++ goalieDetails goalie ++ "\n\n"

      in hdr ++ "Are you sure you want to delete this goalie? (Y/N)"

    return C.CursorInvisible

  , handleController = \e -> do

    case ynHandler e of

      Just True -> do
        gets (^.progMode.editGoalieStateL.egsSelectedGoalie) >>= mapM_
          (\gid -> modify $ database.dbGoalies %~ dropNth gid)
        modify editGoalie

      Just False -> modify editGoalie
      Nothing    -> return ()

    return True

  }

ytdGamesC :: Bool -> Action () -> Controller
ytdGamesC = curry $ promptController .
  uncurry editGoalieYtdGamesPrompt

ytdMinsC :: Bool -> Action () -> Controller
ytdMinsC = curry $ promptController .
  uncurry editGoalieYtdMinsPrompt

ytdGoalsC :: Bool -> Action () -> Controller
ytdGoalsC = curry $ promptController .
  uncurry editGoalieYtdGoalsPrompt

ytdShutoutsC :: Bool -> Action () -> Controller
ytdShutoutsC = curry $ promptController .
  uncurry editGoalieYtdShutoutsPrompt

ytdWinsC :: Bool -> Action () -> Controller
ytdWinsC = curry $ promptController .
  uncurry editGoalieYtdWinsPrompt

ytdLossesC :: Bool -> Action () -> Controller
ytdLossesC = curry $ promptController .
  uncurry editGoalieYtdLossesPrompt

ytdTiesC :: Action () -> Controller
ytdTiesC = promptController . editGoalieYtdTiesPrompt

ltGamesC :: Bool -> Action () -> Controller
ltGamesC = curry $ promptController .
  uncurry editGoalieLtGamesPrompt

ltMinsC :: Bool -> Action () -> Controller
ltMinsC = curry $ promptController .
  uncurry editGoalieLtMinsPrompt

ltGoalsC :: Bool -> Action() -> Controller
ltGoalsC = curry $ promptController .
  uncurry editGoalieLtGoalsPrompt

ltShutoutsC :: Bool -> Action () -> Controller
ltShutoutsC = curry $ promptController .
  uncurry editGoalieLtShutoutsPrompt

ltWinsC :: Bool -> Action () -> Controller
ltWinsC = curry $ promptController .
  uncurry editGoalieLtWinsPrompt

ltLossesC :: Bool -> Action () -> Controller
ltLossesC = curry $ promptController .
  uncurry editGoalieLtLossesPrompt

ltTiesC :: Action () -> Controller
ltTiesC = promptController . editGoalieLtTiesPrompt

header :: ProgState -> C.Update ()
header s = C.drawString $ fromMaybe "" $ do
  gid <- s^.progMode.editGoalieStateL.egsSelectedGoalie
  g   <- nth gid $ s^.database.dbGoalies
  Just $ goalieDetails g ++ "\n"
