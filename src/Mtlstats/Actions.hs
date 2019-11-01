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
  , createGoalie
  , editPlayer
  , addPlayer
  , addGoalie
  , resetCreatePlayerState
  , resetCreateGoalieState
  , recordGoalAssists
  , awardGoal
  , awardAssist
  , resetGoalData
  , assignPMins
  , recordGoalieStats
  , backHome
  , scrollUp
  , scrollDown
  ) where

import Control.Monad.Trans.State (modify)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (fromGregorianValid)
import Lens.Micro (over, (^.), (&), (.~), (?~), (%~), (+~))

import Mtlstats.Types
import Mtlstats.Util

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

-- | Awards the goal and assists to the players
recordGoalAssists :: ProgState -> ProgState
recordGoalAssists ps = fromMaybe ps $ do
  let gs = ps^.progMode.gameStateL
  goalId <- gs^.goalBy
  let assistIds = gs^.assistsBy
  Just $ ps
    & awardGoal goalId
    & (\s -> foldr awardAssist s assistIds)
    & progMode.gameStateL
      %~ (goalBy              .~ Nothing)
      .  (assistsBy           .~ [])
      .  (pointsAccounted     %~ succ)
      .  (confirmGoalDataFlag .~ False)

-- | Awards a goal to a player
awardGoal
  :: Int
  -- ^ The player's index number
  -> ProgState
  -> ProgState
awardGoal n ps = ps
  & progMode.gameStateL.gamePlayerStats %~
    (\m -> let
      stats = M.findWithDefault newPlayerStats n m
      in M.insert n (stats & psGoals %~ succ) m)
  & database.dbPlayers %~ map
    (\(i, p) -> if i == n
      then p
        & pYtd.psGoals      %~ succ
        & pLifetime.psGoals %~ succ
      else p) . zip [0..]

-- | Awards an assist to a player
awardAssist
  :: Int
  -- ^ The player's index number
  -> ProgState
  -> ProgState
awardAssist n ps = ps
  & progMode.gameStateL.gamePlayerStats %~
    (\m -> let
      stats = M.findWithDefault newPlayerStats n m
      in M.insert n (stats & psAssists %~ succ) m)
  & database.dbPlayers %~ map
    (\(i, p) -> if i == n
      then p
        & pYtd.psAssists      %~ succ
        & pLifetime.psAssists %~ succ
      else p) . zip [0..]

-- | Resets the entered data for the current goal
resetGoalData :: ProgState -> ProgState
resetGoalData ps = ps & progMode.gameStateL
  %~ (goalBy              .~ Nothing)
  .  (assistsBy           .~ [])
  .  (confirmGoalDataFlag .~ False)

-- | Adds penalty minutes to a player
assignPMins
  :: Int
  -- ^ The number of minutes to add
  -> ProgState
  -> ProgState
assignPMins mins s = fromMaybe s $ do
  n <- s^.progMode.gameStateL.selectedPlayer
  Just $ s
    & database.dbPlayers %~ modifyNth n
      (((pYtd.psPMin) +~ mins) . ((pLifetime.psPMin) +~ mins))
    & progMode.gameStateL
      %~ ( gamePlayerStats %~ updateMap n newPlayerStats
           (psPMin +~ mins)
         )
      .  (selectedPlayer .~ Nothing)

-- | Records the goalie's game stats
recordGoalieStats :: ProgState -> ProgState
recordGoalieStats s = fromMaybe s $ do
  let gs = s^.progMode.gameStateL
  gid    <- gs^.gameSelectedGoalie
  goalie <- nth gid $ s^.database.dbGoalies
  mins   <- gs^.goalieMinsPlayed
  goals  <- gs^.goalsAllowed

  let
    gameStats = M.findWithDefault newGoalieStats gid $ gs^.gameGoalieStats
    bumpVal   = if gameStats^.gsGames == 0
      then 1
      else 0

    bumpStats gs = gs
      & gsGames        +~ bumpVal
      & gsMinsPlayed   +~ mins
      & gsGoalsAllowed +~ goals

  Just $ s
    & progMode.gameStateL
      %~ (gameGoalieStats    %~ updateMap gid newGoalieStats bumpStats)
      .  (gameSelectedGoalie .~ Nothing)
      .  (goalieMinsPlayed   .~ Nothing)
      .  (goalsAllowed       .~ Nothing)
    & database.dbGoalies
      %~ modifyNth gid (\goalie -> goalie
         & gYtd      %~ bumpStats
         & gLifetime %~ bumpStats)

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
