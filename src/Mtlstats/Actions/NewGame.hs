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

module Mtlstats.Actions.NewGame
  ( overtimeCheck
  , updateGameStats
  , validateGameDate
  , recordGoalAssists
  , awardGoal
  , awardAssist
  , resetGoalData
  , assignPMins
  , awardShutouts
  ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (fromGregorianValid)
import Lens.Micro ((^.), (&), (.~), (?~), (%~), (+~))

import Mtlstats.Types
import Mtlstats.Util

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
  & database.dbPlayers %~ zipWith
    (\i p -> if i == n
      then p
        & pYtd.psGoals      %~ succ
        & pLifetime.psGoals %~ succ
      else p)
    [0..]

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
  & database.dbPlayers %~ zipWith
    (\i p -> if i == n
      then p
        & pYtd.psAssists      %~ succ
        & pLifetime.psAssists %~ succ
      else p)
    [0..]

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
  n <- s^.progMode.gameStateL.gameSelectedPlayer
  Just $ s
    & database.dbPlayers %~ modifyNth n
      (((pYtd.psPMin) +~ mins) . ((pLifetime.psPMin) +~ mins))
    & progMode.gameStateL
      %~ ( gamePlayerStats %~ updateMap n newPlayerStats
           (psPMin +~ mins)
         )
      .  (gameSelectedPlayer .~ Nothing)

-- | Awards a shutout to any 'Goalie' who played and didn't allow any
-- goals
awardShutouts :: ProgState -> ProgState
awardShutouts s = foldl
  (\s' (gid, stats) -> if stats^.gsGoalsAllowed == 0
    then s'
      & database.dbGoalies %~ modifyNth gid
        ( ( gYtd.gsShutouts      %~ succ )
        . ( gLifetime.gsShutouts %~ succ )
        )
      & progMode.gameStateL.gameGoalieStats %~ M.adjust
        (gsShutouts %~ succ)
        gid
    else s')
  s
  (M.toList $ s^.progMode.gameStateL.gameGoalieStats)
