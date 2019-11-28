{-

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

module Actions.NewGameSpec (spec) where

import Control.Monad (replicateM)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Lens.Micro ((^.), (&), (.~), (?~), (%~))
import Test.Hspec (Spec, context, describe, it, runIO, shouldBe)

import Mtlstats.Actions.NewGame
import Mtlstats.Types
import Mtlstats.Util

import qualified Actions.NewGame.GoalieInputSpec as GoalieInput
import qualified TypesSpec as TS

spec :: Spec
spec = describe "NewGame" $ do
  overtimeCheckSpec
  updateGameStatsSpec
  validateGameDateSpec
  recordGoalAssistsSpec
  awardGoalSpec
  awardAssistSpec
  resetGoalDataSpec
  assignPMinsSpec
  awardShutoutsSpec
  GoalieInput.spec

overtimeCheckSpec :: Spec
overtimeCheckSpec = describe "overtimeCheck" $ do

  context "tie game" $ do
    let
      s = newProgState
        & progMode.gameStateL
          %~ (gameType  ?~ HomeGame)
          .  (homeScore ?~ 1)
          .  (awayScore ?~ 1)
        & overtimeCheck

    it "should clear the home score" $
      s^.progMode.gameStateL.homeScore `shouldBe` Nothing

    it "should clear the away score" $
      s^.progMode.gameStateL.awayScore `shouldBe` Nothing

    it "should leave the overtimeFlag blank" $
      s^.progMode.gameStateL.overtimeFlag `shouldBe` Nothing

  context "game won" $ do
    let
      s = newProgState
        & progMode.gameStateL
          %~ (gameType  ?~ HomeGame)
          .  (homeScore ?~ 2)
          .  (awayScore ?~ 1)
        & overtimeCheck

    it "should not change the home score" $
      s^.progMode.gameStateL.homeScore `shouldBe` Just 2

    it "should not change the away score" $
      s^.progMode.gameStateL.awayScore `shouldBe` Just 1

    it "should set the overtimeCheck flag to False" $
      s^.progMode.gameStateL.overtimeFlag `shouldBe` Just False

  context "game lost" $ do
    let
      s = newProgState
        & progMode.gameStateL
          %~ (gameType  ?~ HomeGame)
          .  (homeScore ?~ 1)
          .  (awayScore ?~ 2)
        & overtimeCheck

    it "should not change the home score" $
      s^.progMode.gameStateL.homeScore `shouldBe` Just 1

    it "should not change the away score" $
      s^.progMode.gameStateL.awayScore `shouldBe` Just 2

    it "should leave the overtimeCheck flag blank" $
      s^.progMode.gameStateL.overtimeFlag `shouldBe` Nothing

updateGameStatsSpec :: Spec
updateGameStatsSpec = describe "updateGameStats" $ do
  let

    baseStats = newGameStats
      & gmsWins         .~ 1
      & gmsLosses       .~ 1
      & gmsOvertime     .~ 1
      & gmsGoalsFor     .~ 1
      & gmsGoalsAgainst .~ 1

    s t h a o = newProgState
      & progMode.gameStateL
        %~ (gameType     .~ t)
        .  (homeScore    .~ h)
        .  (awayScore    .~ a)
        .  (overtimeFlag .~ o)
      & database
        %~ (dbHomeGameStats .~ baseStats)
        .  (dbAwayGameStats .~ baseStats)

    db hw hl ho hf ha aw al ao af aa = newDatabase
      & dbHomeGameStats
        %~ (gmsWins         .~ hw)
        .  (gmsLosses       .~ hl)
        .  (gmsOvertime     .~ ho)
        .  (gmsGoalsFor     .~ hf)
        .  (gmsGoalsAgainst .~ ha)
      & dbAwayGameStats
        %~ (gmsWins         .~ aw)
        .  (gmsLosses       .~ al)
        .  (gmsOvertime     .~ ao)
        .  (gmsGoalsFor     .~ af)
        .  (gmsGoalsAgainst .~ aa)

  context "home win" $
    it "should record a home win" $ let
      s'  = s (Just HomeGame) (Just 2) (Just 1) (Just False)
      db' = updateGameStats s' ^. database
      in db' `shouldBe` db 2 1 1 3 2 1 1 1 1 1

  context "home loss" $
    it "should record a home loss" $ let
      s'  = s (Just HomeGame) (Just 1) (Just 2) (Just False)
      db' = updateGameStats s' ^. database
      in db' `shouldBe` db 1 2 1 2 3 1 1 1 1 1

  context "home overtime loss" $
    it "should record a home overtime" $ let
      s'  = s (Just HomeGame) (Just 1) (Just 2) (Just True)
      db' = updateGameStats s' ^. database
      in db' `shouldBe` db 1 1 2 2 3 1 1 1 1 1

  context "away win" $
    it "should record an away win" $ let
      s'  = s (Just AwayGame) (Just 1) (Just 2) (Just False)
      db' = updateGameStats s' ^. database
      in db' `shouldBe` db 1 1 1 1 1 2 1 1 3 2

  context "away loss" $
    it "should record an away loss" $ let
      s'  = s (Just AwayGame) (Just 2) (Just 1) (Just False)
      db' = updateGameStats s' ^. database
      in db' `shouldBe` db 1 1 1 1 1 1 2 1 2 3

  context "away overtime loss" $
    it "should record an away overtime" $ let
      s'  = s (Just AwayGame) (Just 2) (Just 1) (Just True)
      db' = updateGameStats s' ^. database
      in db' `shouldBe` db 1 1 1 1 1 1 1 2 2 3

  context "missing game type" $
    it "should not change anything" $ let
      s'  = s Nothing (Just 1) (Just 2) (Just True)
      db' = updateGameStats s' ^. database
      in db' `shouldBe` db 1 1 1 1 1 1 1 1 1 1

  context "missing home score" $
    it "should not change anything" $ let
      s'  = s (Just HomeGame) Nothing (Just 1) (Just True)
      db' = updateGameStats s' ^. database
      in db' `shouldBe` db 1 1 1 1 1 1 1 1 1 1

  context "missing away score" $
    it "should not change anything" $ let
      s'  = s (Just HomeGame) (Just 1) Nothing (Just True)
      db' = updateGameStats s' ^. database
      in db' `shouldBe` db 1 1 1 1 1 1 1 1 1 1

  context "missing overtime flag" $
    it "should not change anything" $ let
      s'  = s (Just HomeGame) (Just 1) (Just 2) Nothing
      db' = updateGameStats s' ^. database
      in db' `shouldBe` db 1 1 1 1 1 1 1 1 1 1

validateGameDateSpec :: Spec
validateGameDateSpec = describe "validateGameDate" $ do

  context "valid date" $
    it "should leave the date unchanged" $ do
      let
        s = newProgState
          & progMode.gameStateL
            %~ (gameYear  ?~ 2019)
            .  (gameMonth ?~ 6)
            .  (gameDay   ?~ 25)
          & validateGameDate
      s^.progMode.gameStateL.gameYear  `shouldBe` Just 2019
      s^.progMode.gameStateL.gameMonth `shouldBe` Just 6
      s^.progMode.gameStateL.gameDay   `shouldBe` Just 25

  context "invalid date" $
    it "should clear the date" $ do
      let
        s = newProgState
          & progMode.gameStateL
            %~ (gameYear  ?~ 2019)
            .  (gameMonth ?~ 2)
            .  (gameDay   ?~ 30)
          & validateGameDate
      s^.progMode.gameStateL.gameYear  `shouldBe` Nothing
      s^.progMode.gameStateL.gameMonth `shouldBe` Nothing
      s^.progMode.gameStateL.gameDay   `shouldBe` Nothing

  context "missing day" $
    it "should not change anything" $ do
      let

        gs = newGameState
          & gameYear  ?~ 2019
          & gameMonth ?~ 6

        s = newProgState
          & progMode.gameStateL .~ gs
          & validateGameDate

      s^.progMode.gameStateL.gameYear  `shouldBe` Just 2019
      s^.progMode.gameStateL.gameMonth `shouldBe` Just 6
      s^.progMode.gameStateL.gameDay   `shouldBe` Nothing

recordGoalAssistsSpec :: Spec
recordGoalAssistsSpec = describe "recordGoalAssists" $ do
  let
    joe   = newPlayer 1 "Joe"   "centre"
    bob   = newPlayer 2 "Bob"   "defense"
    steve = newPlayer 3 "Steve" "forward"
    dave  = newPlayer 4 "Dave"  "somewhere"
    frank = newPlayer 5 "Frank" "elsewhere"
    ps
      = newProgState
      & database.dbPlayers .~ [joe, bob, steve, dave, frank]
      & progMode.gameStateL
        %~ (goalBy              ?~ 0)
        .  (assistsBy           .~ [1, 2])
        .  (confirmGoalDataFlag .~ True)
      & recordGoalAssists

  mapM_
    (\(name, n, goals, assists) -> context name $ do
      let
        player = (ps^.database.dbPlayers) !! n
        stats  = M.findWithDefault newPlayerStats n $
          ps^.progMode.gameStateL.gamePlayerStats

      it ("should set the year-to-date goals to " ++ show goals) $
        player^.pYtd.psGoals `shouldBe` goals

      it ("should set the lifetime goals to " ++ show goals) $
        player^.pLifetime.psGoals `shouldBe` goals

      it ("should set the game goals to " ++ show goals) $
        stats^.psAssists `shouldBe` assists

      it ("should set the year-to-date assists to " ++ show assists) $
        player^.pYtd.psAssists `shouldBe` assists

      it ("should set the lifetime assists to " ++ show assists) $
        player^.pLifetime.psAssists `shouldBe` assists

      it ("should set the game assists to " ++ show assists) $
        stats^.psAssists `shouldBe` assists)

    --  name,    index, goals, assists
    [ ( "Joe",   0,     1,     0       )
    , ( "Bob",   1,     0,     1       )
    , ( "Steve", 2,     0,     1       )
    , ( "Dave",  3,     0,     0       )
    ]

  it "should clear the goalBy value" $
    ps^.progMode.gameStateL.goalBy `shouldBe` Nothing

  it "should clear the assistsBy list" $
    ps^.progMode.gameStateL.assistsBy `shouldBe` []

  it "should increment the pointsAccounted counter" $
    ps^.progMode.gameStateL.pointsAccounted `shouldBe` 1

  it "should clear the confirmGoalDataFlag" $
    ps^.progMode.gameStateL.confirmGoalDataFlag `shouldBe` False

awardGoalSpec :: Spec
awardGoalSpec = describe "awardGoal" $ do
  let
    joe
      = newPlayer 2 "Joe" "centre"
      & pYtd.psGoals      .~ 1
      & pLifetime.psGoals .~ 2
    bob
      = newPlayer 3 "Bob" "defense"
      & pYtd.psGoals      .~ 3
      & pLifetime.psGoals .~ 4
    db
      = newDatabase
      & dbPlayers .~ [joe, bob]
    joeStats
      = newPlayerStats
      & psGoals .~ 1
    ps
      = newProgState
      & progMode.gameStateL.gamePlayerStats .~ M.singleton 0 joeStats
      & database .~ db

  mapM_
    (\(name, pid, ytd, lt, game) ->
      context name $ do
        let
          ps'    = awardGoal pid ps
          player = (ps'^.database.dbPlayers) !! pid
          gStats = (ps'^.progMode.gameStateL.gamePlayerStats) M.! pid

        it ("should increment " ++ name ++ "'s year-to-date goals") $
          player^.pYtd.psGoals `shouldBe` ytd

        it ("should increment " ++ name ++ "'s lifetime goals") $
          player^.pLifetime.psGoals `shouldBe` lt

        it ("should increment " ++ name ++ "'s game goals") $
          gStats^.psGoals `shouldBe` game)
    --  player name, player id, ytd goals, lifetime goals, game goals
    [ ( "Joe",       0,         2,         3,              2          )
    , ( "Bob",       1,         4,         5,              1          )
    ]

  context "invalid index" $ let
    ps' = awardGoal 2 ps
    in it "should not change the database" $
      ps'^.database `shouldBe` db

  context "negative index" $ let
    ps' = awardGoal (-1) ps
    in it "should not change the database" $
      ps'^.database `shouldBe` db

awardAssistSpec :: Spec
awardAssistSpec = describe "awardAssist" $ do
  let
    joe
      = newPlayer 1 "Joe" "centre"
      & pYtd.psAssists      .~ 1
      & pLifetime.psAssists .~ 2
    bob
      = newPlayer 2 "Bob" "defense"
      & pYtd.psAssists      .~ 3
      & pLifetime.psAssists .~ 4
    joeStats
      = newPlayerStats
      & psAssists .~ 1
    ps
      = newProgState
      & progMode.gameStateL.gamePlayerStats .~ M.singleton 0 joeStats
      & database.dbPlayers .~ [joe, bob]

  mapM_
    (\(name, pid, ytd, lt, game) ->
      context name $ do
        let
          ps' = awardAssist pid ps
          player = (ps'^.database.dbPlayers) !! pid
          gStats = (ps'^.progMode.gameStateL.gamePlayerStats) M.! pid

        it ("should increment " ++ name ++ "'s year-to-date assists") $
          player^.pYtd.psAssists `shouldBe` ytd

        it ("should increment " ++ name ++ "'s lifetime assists") $
          player^.pLifetime.psAssists `shouldBe` lt

        it ("should increment " ++ name ++ "'s game assists") $
          gStats^.psAssists `shouldBe` game)
    --  player name, player id, ytd assists, lifetime assists, game assists
    [ ( "Joe",       0,         2,           3,                2            )
    , ( "Bob",       1,         4,           5,                1            )
    ]

  context "invalid index" $ let
    ps' = awardAssist (-1) ps
    in it "should not change anything" $
      ps'^.database.dbPlayers `shouldBe` ps^.database.dbPlayers

resetGoalDataSpec :: Spec
resetGoalDataSpec = describe "resetGoalData" $ do
  players <- runIO $ replicateM 5 TS.makePlayer
  let
    gs
      = newGameState
      & goalBy              ?~ 1
      & assistsBy           .~ [2, 3]
      & confirmGoalDataFlag .~ True
    ps
      = newProgState
      & database.dbPlayers  .~ players
      & progMode.gameStateL .~ gs
      & resetGoalData

  it "should clear the goalBy value" $
    ps^.progMode.gameStateL.goalBy `shouldBe` Nothing

  it "should clear the assists by list" $
    ps^.progMode.gameStateL.assistsBy `shouldBe` []

  it "should clear confirmGoalDataFlag" $
    ps^.progMode.gameStateL.confirmGoalDataFlag `shouldBe` False

assignPMinsSpec :: Spec
assignPMinsSpec = describe "assignPMins" $ let

  bob = newPlayer 2 "Bob" "centre"
    & pYtd.psPMin      .~ 3
    & pLifetime.psPMin .~ 4

  joe = newPlayer 3 "Joe" "defense"
    & pYtd.psPMin      .~ 5
    & pLifetime.psPMin .~ 6

  ps pid = newProgState
    & database.dbPlayers .~ [bob, joe]
    & progMode.gameStateL
      %~ (gamePlayerStats .~ M.fromList [(0, newPlayerStats & psPMin .~ 2)])
      .  (gameSelectedPlayer .~ pid)

  in mapM_
    (\(pid, bobLt, bobYtd, bobGame, joeLt, joeYtd, joeGame) ->
      context ("selectedPlayer = " ++ show pid) $ do
        let ps' = assignPMins 2 $ ps pid

        mapM_
          (\(name, pid', lt, ytd, game) -> context name $ do
            let
              player = fromJust $ nth pid' $ ps'^.database.dbPlayers
              gStats = ps'^.progMode.gameStateL.gamePlayerStats
              pStats = M.findWithDefault newPlayerStats pid' gStats

            context "lifetime penalty minutes" $
              it ("should be " ++ show lt) $
                player^.pLifetime.psPMin `shouldBe` lt

            context "year-to-date penalty minutes" $
              it ("should be " ++ show ytd) $
                player^.pYtd.psPMin `shouldBe` ytd

            context "game penalty minutes" $
              it ("should be " ++ show game) $
                pStats^.psPMin `shouldBe` game)

          --  name,  index, lifetime, ytd,    game
          [ ( "Bob", 0,     bobLt,    bobYtd, bobGame )
          , ( "Joe", 1,     joeLt,    joeYtd, joeGame )
          ]

        it "should set selectedPlayer to Nothing" $
          ps'^.progMode.gameStateL.gameSelectedPlayer `shouldBe` Nothing)

    --  index,   bob lt, bob ytd, bob game, joe lt, joe ytd, joe game
    [ ( Just 0,  6,      5,       4,        6,      5,       0        )
    , ( Just 1,  4,      3,       2,        8,      7,       2        )
    , ( Just 2,  4,      3,       2,        6,      5,       0        )
    , ( Nothing, 4,      3,       2,        6,      5,       0        )
    ]

awardShutoutsSpec :: Spec
awardShutoutsSpec = describe "awardShutouts" $ let
  joe = newGoalie 2 "Joe"
    & gYtd.gsShutouts      .~ 1
    & gLifetime.gsShutouts .~ 2

  bob = newGoalie 3 "Bob"
    & gYtd.gsShutouts      .~ 3
    & gLifetime.gsShutouts .~ 4

  steve = newGoalie 5 "Steve"
    & gYtd.gsShutouts      .~ 5
    & gLifetime.gsShutouts .~ 6

  ps = newProgState
    & database.dbGoalies .~ [joe, bob, steve]
    & progMode.gameStateL.gameGoalieStats .~ M.fromList
      [ ( 0, newGoalieStats & gsGoalsAllowed .~ 1 )
      , ( 1, newGoalieStats                       )
      ]
    & awardShutouts

  in mapM_
    (\(name, gid, expectedGame, expectedYtd, expectedLt) -> context name $ let
      game = M.findWithDefault newGoalieStats gid $
        ps^.progMode.gameStateL.gameGoalieStats
      goalie = (ps^.database.dbGoalies) !! gid
      in mapM_
        (\(label, actual, expected) -> context label $
          it ("should be " ++ show actual) $
            actual `shouldBe` expected)
        --  label,      actual,                       expected
        [ ( "Game",     game^.gsShutouts,             expectedGame )
        , ( "YTD",      goalie^.gYtd.gsShutouts,      expectedYtd  )
        , ( "lifetime", goalie^.gLifetime.gsShutouts, expectedLt   )
        ])
    --  goalie,  goalie ID, Game, YTD, lifetime
    [ ( "Joe",   0,         0,    1,   2        )
    , ( "Bob",   1,         1,    4,   5        )
    , ( "Steve", 2,         0,    5,   6        )
    ]
