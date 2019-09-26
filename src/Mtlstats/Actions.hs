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
  , overtimeCheck
  , updateGameStats
  , validateGameDate
  , createPlayer
  , addPlayer
  , recordGoalAssists
  , awardGoal
  ) where

import Control.Monad.Trans.State (modify)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (fromGregorianValid)
import Lens.Micro (over, (^.), (&), (.~), (?~), (%~), (+~))

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

-- | Determines whether or not to perform a check for overtime
overtimeCheck :: ProgState -> ProgState
overtimeCheck s
  | fromMaybe False $ gameTied $ s^.progMode.gameStateL =
    s & progMode.gameStateL
    %~ (homeScore .~ Nothing)
    .  (awayScore .~ Nothing)
  | fromMaybe False $ gameWon $ s^.progMode.gameStateL =
    s & progMode.gameStateL.overtimeFlag ?~ False
  | otherwise  = s

-- | Adjusts the game stats based on the results of the current game
updateGameStats :: ProgState -> ProgState
updateGameStats s = fromMaybe s $ do
  let gs = s^.progMode.gameStateL
  gType  <- gs^.gameType
  won    <- gameWon gs
  lost   <- gameLost gs
  ot     <- gs^.overtimeFlag
  tScore <- teamScore gs
  oScore <- otherScore gs
  let
    hw  = if gType == HomeGame && won then 1 else 0
    hl  = if gType == HomeGame && lost then 1 else 0
    hot = if gType == HomeGame && ot then 1 else 0
    hgf = if gType == HomeGame then tScore else 0
    hga = if gType == HomeGame then oScore else 0
    aw  = if gType == AwayGame && won then 1 else 0
    al  = if gType == AwayGame && lost then 1 else 0
    aot = if gType == AwayGame && ot then 1 else 0
    agf = if gType == AwayGame then tScore else 0
    aga = if gType == AwayGame then oScore else 0
  Just $ s
    & database.dbHomeGameStats
      %~ (gmsWins         +~ hw)
      .  (gmsLosses       +~ hl)
      .  (gmsOvertime     +~ hot)
      .  (gmsGoalsFor     +~ hgf)
      .  (gmsGoalsAgainst +~ hga)
    & database.dbAwayGameStats
      %~ (gmsWins         +~ aw)
      .  (gmsLosses       +~ al)
      .  (gmsOvertime     +~ aot)
      .  (gmsGoalsFor     +~ agf)
      .  (gmsGoalsAgainst +~ aga)

-- | Validates the game date
validateGameDate :: ProgState -> ProgState
validateGameDate s = fromMaybe s $ do
  y <- toInteger <$> s^.progMode.gameStateL.gameYear
  m <- s^.progMode.gameStateL.gameMonth
  d <- s^.progMode.gameStateL.gameDay
  Just $ if null $ fromGregorianValid y m d
    then s & progMode.gameStateL
      %~ (gameYear  .~ Nothing)
      .  (gameMonth .~ Nothing)
      .  (gameDay   .~ Nothing)
    else s

-- | Starts player creation mode
createPlayer :: ProgState -> ProgState
createPlayer = let
  cb = modify $ progMode .~ MainMenu
  cps
    = newCreatePlayerState
    & cpsSuccessCallback .~ cb
    & cpsFailureCallback .~ cb
  in progMode .~ CreatePlayer cps

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
    %~ (player:)

-- | Awards the goal and assists to the players
recordGoalAssists :: ProgState -> ProgState
recordGoalAssists = undefined

-- | Awards a goal to a player
awardGoal
  :: Int
  -- ^ The player's index number
  -> ProgState
  -> ProgState
awardGoal n ps = ps
  &  database.dbPlayers
  %~ map
     (\(i, p) -> if i == n
       then p
         & pYtd.psGoals      %~ succ
         & pLifetime.psGoals %~ succ
       else p) . zip [0..]
