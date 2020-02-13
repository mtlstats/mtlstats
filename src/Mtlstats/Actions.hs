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

module Mtlstats.Actions
  ( startNewSeason
  , resetYtd
  , clearRookies
  , resetStandings
  , startNewGame
  , addChar
  , removeChar
  , createPlayer
  , createGoalie
  , edit
  , editPlayer
  , editSelectedPlayer
  , editGoalie
  , editSelectedGoalie
  , addPlayer
  , addGoalie
  , resetCreatePlayerState
  , resetCreateGoalieState
  , backHome
  , scrollUp
  , scrollDown
  , saveDatabase
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (gets, modify)
import Data.Aeson (encodeFile)
import Data.Maybe (fromMaybe)
import Lens.Micro ((^.), (&), (.~), (%~))
import System.EasyFile
  ( createDirectoryIfMissing
  , getAppUserDataDirectory
  , (</>)
  )

import Mtlstats.Config
import Mtlstats.Types
import Mtlstats.Util

-- | Starts a new season
startNewSeason :: ProgState -> ProgState
startNewSeason
  = (progMode .~ NewSeason False)
  . (database.dbGames .~ 0)

-- | Resets all players year-to-date stats
resetYtd :: ProgState -> ProgState
resetYtd
  = (database . dbPlayers %~ map (pYtd .~ newPlayerStats))
  . (database . dbGoalies %~ map (gYtd .~ newGoalieStats))

-- | Clears the rookie flag from all players/goalies
clearRookies :: ProgState -> ProgState
clearRookies = database
  %~ (dbPlayers %~ map (pRookie .~ False))
  .  (dbGoalies %~ map (gRookie .~ False))

-- | Resets game standings
resetStandings :: ProgState -> ProgState
resetStandings = database
  %~ ( dbHomeGameStats .~ newGameStats)
  .  ( dbAwayGameStats .~ newGameStats)

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
  callback = modify edit
  cps = newCreatePlayerState
    & cpsSuccessCallback .~ callback
    & cpsFailureCallback .~ callback
  in progMode .~ CreatePlayer cps

-- | Starts goalie creation mode
createGoalie :: ProgState -> ProgState
createGoalie = let
  callback = modify edit
  cgs = newCreateGoalieState
    & cgsSuccessCallback .~ callback
    & cgsFailureCallback .~ callback
  in progMode .~ CreateGoalie cgs

-- | Launches the edit menu
edit :: ProgState -> ProgState
edit = progMode .~ EditMenu

-- | Starts the player editing process
editPlayer :: ProgState -> ProgState
editPlayer = progMode .~ EditPlayer newEditPlayerState

-- | Edits the selected 'Player'
editSelectedPlayer
  :: (Player -> Player)
  -- ^ The modification to be made to the 'Player'
  -> ProgState
  -> ProgState
editSelectedPlayer f s = fromMaybe s $ do
  n <- s^.progMode.editPlayerStateL.epsSelectedPlayer
  let
    players  = s^.database.dbPlayers
    players' = modifyNth n f players
  Just $ s & database.dbPlayers .~ players'

-- | Starts the 'Goalie' editing process
editGoalie :: ProgState -> ProgState
editGoalie = progMode .~ EditGoalie newEditGoalieState

-- | Edits the selected 'Goalie'
editSelectedGoalie
  :: (Goalie -> Goalie)
  -- ^ The modification to be made to the 'Goalie'
  -> ProgState
  -> ProgState
editSelectedGoalie f s = fromMaybe s $ do
  n <- s^.progMode.editGoalieStateL.egsSelectedGoalie
  let
    goalies  = s^.database.dbGoalies
    goalies' = modifyNth n f goalies
  Just $ s & database.dbGoalies .~ goalies'

-- | Adds the entered player to the roster
addPlayer :: ProgState -> ProgState
addPlayer s = fromMaybe s $ do
  let cps = s^.progMode.createPlayerStateL
  num   <- cps^.cpsNumber
  rFlag <- cps^.cpsRookieFlag
  let
    name   = cps^.cpsName
    pos    = cps^.cpsPosition
    player = newPlayer num name pos
      & pRookie .~ rFlag
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

-- | Saves the database
saveDatabase :: String -> Action ()
saveDatabase fn = do
  db <- gets (^.database)
  liftIO $ do
    dir <- getAppUserDataDirectory appName
    let dbFile = dir </> fn
    createDirectoryIfMissing True dir
    encodeFile dbFile db
