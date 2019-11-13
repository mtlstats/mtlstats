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

{-# LANGUAGE LambdaCase #-}

module Mtlstats.Actions
  ( startNewSeason
  , resetYtd
  , startNewGame
  , addChar
  , removeChar
  , createPlayer
  , createGoalie
  , editPlayer
  , editGoalie
  , addPlayer
  , addGoalie
  , resetCreatePlayerState
  , resetCreateGoalieState
  , backHome
  , scrollUp
  , scrollDown
  ) where

import Control.Monad.Trans.State (modify)
import Data.Maybe (fromMaybe)
import Lens.Micro ((^.), (&), (.~), (%~))

import Mtlstats.Types

-- | Starts a new season
startNewSeason :: ProgState -> ProgState
startNewSeason = (progMode .~ NewSeason) . (database . dbGames .~ 0)

-- | Resets all players year-to-date stats
resetYtd :: ProgState -> ProgState
resetYtd
  = (database . dbPlayers %~ map (pYtd .~ newPlayerStats))
  . (database . dbGoalies %~ map (gYtd .~ newGoalieStats))

-- | Starts a new game
startNewGame :: ProgState -> ProgState
startNewGame
  = (progMode .~ NewGame newGameState)
  . (database . dbGames %~ succ)

-- | Adds a character to the input buffer
addChar :: Char -> ProgState -> ProgState
addChar c = inputBuffer %~ (++[c])

-- | Removes a character from the input buffer (if possible)
removeChar :: ProgState -> ProgState
removeChar = inputBuffer %~ \case
  ""  -> ""
  str -> init str

-- | Starts player creation mode
createPlayer :: ProgState -> ProgState
createPlayer = let
  callback = modify $ progMode .~ MainMenu
  cps = newCreatePlayerState
    & cpsSuccessCallback .~ callback
    & cpsFailureCallback .~ callback
  in progMode .~ CreatePlayer cps

-- | Starts goalie creation mode
createGoalie :: ProgState -> ProgState
createGoalie = let
  callback = modify $ progMode .~ MainMenu
  cgs = newCreateGoalieState
    & cgsSuccessCallback .~ callback
    & cgsFailureCallback .~ callback
  in progMode .~ CreateGoalie cgs

-- | Starts the player editing process
editPlayer :: ProgState -> ProgState
editPlayer = progMode .~ EditPlayer newEditPlayerState

-- | Starts the 'Goalie' editing process
editGoalie :: ProgState -> ProgState
editGoalie = undefined

-- | Adds the entered player to the roster
addPlayer :: ProgState -> ProgState
addPlayer s = fromMaybe s $ do
  let cps = s^.progMode.createPlayerStateL
  num <- cps^.cpsNumber
  let
    name   = cps^.cpsName
    pos    = cps^.cpsPosition
    player = newPlayer num name pos
  Just $ s & database.dbPlayers
    %~ (++[player])

-- | Adds the entered goalie to the roster
addGoalie :: ProgState -> ProgState
addGoalie s = fromMaybe s $ do
  let cgs = s^.progMode.createGoalieStateL
  num <- cgs^.cgsNumber
  let
    name   = cgs^.cgsName
    goalie = newGoalie num name
  Just $ s & database.dbGoalies
    %~ (++[goalie])

-- | Resets the 'CreatePlayerState' value
resetCreatePlayerState :: ProgState -> ProgState
resetCreatePlayerState = progMode.createPlayerStateL
  %~ (cpsNumber   .~ Nothing)
  .  (cpsName     .~ "")
  .  (cpsPosition .~ "")

-- | Resets the 'CreateGoalieState' value
resetCreateGoalieState :: ProgState -> ProgState
resetCreateGoalieState = progMode.createGoalieStateL
  %~ (cgsNumber .~ Nothing)
  .  (cgsName   .~ "")

-- | Resets the program state back to the main menu
backHome :: ProgState -> ProgState
backHome
  = (progMode     .~ MainMenu)
  . (inputBuffer  .~ "")
  . (scrollOffset .~ 0)

-- | Scrolls the display up
scrollUp :: ProgState -> ProgState
scrollUp = scrollOffset %~ max 0 . pred

-- | Scrolls the display down
scrollDown :: ProgState -> ProgState
scrollDown = scrollOffset %~ succ
