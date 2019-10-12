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

{-# LANGUAGE LambdaCase #-}

module ActionsSpec (spec) where

import Control.Monad (replicateM)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Lens.Micro ((^.), (&), (.~), (?~), (%~))
import System.Random (randomRIO)
import Test.Hspec
  ( Spec
  , context
  , describe
  , it
  , runIO
  , shouldBe
  , shouldNotBe
  , shouldSatisfy
  )

import Mtlstats.Actions
import Mtlstats.Types
import Mtlstats.Util

spec :: Spec
spec = describe "Mtlstats.Actions" $ do
  startNewSeasonSpec
  startNewGameSpec
  resetYtdSpec
  addCharSpec
  removeCharSpec
  overtimeCheckSpec
  updateGameStatsSpec
  validateGameDateSpec
  createPlayerSpec
  addPlayerSpec
  recordGoalAssistsSpec
  awardGoalSpec
  awardAssistSpec
  resetGoalDataSpec
  assignPMinsSpec
  backHomeSpec

startNewSeasonSpec :: Spec
startNewSeasonSpec = describe "startNewSeason" $ do
  let
    s = newProgState
      & database . dbGames .~ 1
      & startNewSeason

  it "should set the progState to NewSeason" $
    show (s^.progMode) `shouldBe` "NewSeason"

  it "should set the number of games to 0" $
    s ^. database . dbGames `shouldBe` 0

startNewGameSpec :: Spec
startNewGameSpec = describe "startNewGame" $ do
  let s = startNewGame newProgState

  it "should increment the number of games" $
    s ^. database . dbGames `shouldBe` 1

  it "should set the mode to NewGame" $
    show (s^.progMode) `shouldBe` "NewGame"

resetYtdSpec :: Spec
resetYtdSpec = describe "resetYtd" $
  it "should reset the year-to-date stats for all players" $ do
    ps <- replicateM 2 makePlayer
    gs <- replicateM 2 makeGoalie
    let
      s = newProgState
        & database . dbPlayers .~ ps
        & database . dbGoalies .~ gs
        & resetYtd
    mapM_
      (\p -> do
        let
          ytd = p ^. pYtd
          lt  = p ^. pLifetime
        ytd ^. psGoals   `shouldBe`    0
        ytd ^. psAssists `shouldBe`    0
        ytd ^. psPMin    `shouldBe`    0
        lt ^. psGoals    `shouldNotBe` 0
        lt ^. psAssists  `shouldNotBe` 0
        lt ^. psPMin     `shouldNotBe` 0) $
      s ^. database . dbPlayers
    mapM_
      (\g -> do
        let
          ytd = g ^. gYtd
          lt  = g ^. gLifetime
        ytd ^. gsGames        `shouldBe`    0
        ytd ^. gsMinsPlayed   `shouldBe`    0
        ytd ^. gsGoalsAllowed `shouldBe`    0
        ytd ^. gsGoalsAgainst `shouldBe`    0
        ytd ^. gsWins         `shouldBe`    0
        ytd ^. gsLosses       `shouldBe`    0
        ytd ^. gsTies         `shouldBe`    0
        lt ^. gsGames         `shouldNotBe` 0
        lt ^. gsMinsPlayed    `shouldNotBe` 0
        lt ^. gsGoalsAllowed  `shouldNotBe` 0
        lt ^. gsGoalsAgainst  `shouldNotBe` 0
        lt ^. gsWins          `shouldNotBe` 0
        lt ^. gsLosses        `shouldNotBe` 0
        lt ^. gsTies          `shouldNotBe` 0) $
      s ^. database . dbGoalies

addCharSpec :: Spec
addCharSpec = describe "addChar" $
  it "should add the character to the input buffer" $ let
    s = newProgState
      & inputBuffer .~ "foo"
      & addChar 'd'
  in s ^. inputBuffer `shouldBe` "food"

removeCharSpec :: Spec
removeCharSpec = describe "removeChar" $ do

  context "empty" $
    it "should remove the character from the input buffer" $ let
      s = removeChar newProgState
      in s ^. inputBuffer `shouldBe` ""

  context "not empty" $
    it "should remove the character from the input buffer" $ let
      s = newProgState
        & inputBuffer .~ "foo"
        & removeChar
      in s ^. inputBuffer `shouldBe` "fo"

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

createPlayerSpec :: Spec
createPlayerSpec = describe "createPlayer" $
  it "should change the mode appropriately" $ let
    s = createPlayer newProgState
    in show (s^.progMode) `shouldBe` "CreatePlayer"

addPlayerSpec :: Spec
addPlayerSpec = describe "addPlayer" $ do
  let
    p1 = newPlayer 1 "Joe" "centre"
    p2 = newPlayer 2 "Bob" "defense"
    db = newDatabase
      & dbPlayers .~ [p1]
    s pm = newProgState
      & progMode .~ pm
      & database .~ db

  context "data available" $
    it "should create the player" $ let
      s' = addPlayer $ s $ CreatePlayer $ newCreatePlayerState
        & cpsNumber   ?~ 2
        & cpsName     .~ "Bob"
        & cpsPosition .~ "defense"
      in s'^.database.dbPlayers `shouldBe` [p1, p2]

  context "data unavailable" $
    it "should not create the player" $ let
      s' = addPlayer $ s MainMenu
      in s'^.database.dbPlayers `shouldBe` [p1]

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
    (\(pName, pid, ytd, lt, game) ->
      context pName $ do
        let
          ps'    = awardGoal pid ps
          player = (ps'^.database.dbPlayers) !! pid
          gStats = (ps'^.progMode.gameStateL.gamePlayerStats) M.! pid

        it ("should increment " ++ pName ++ "'s year-to-date goals") $
          player^.pYtd.psGoals `shouldBe` ytd

        it ("should increment " ++ pName ++ "'s lifetime goals") $
          player^.pLifetime.psGoals `shouldBe` lt

        it ("should increment " ++ pName ++ "'s game goals") $
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
    (\(pName, pid, ytd, lt, game) ->
      context pName $ do
        let
          ps' = awardAssist pid ps
          player = (ps'^.database.dbPlayers) !! pid
          gStats = (ps'^.progMode.gameStateL.gamePlayerStats) M.! pid

        it ("should increment " ++ pName ++ "'s year-to-date assists") $
          player^.pYtd.psAssists `shouldBe` ytd

        it ("should increment " ++ pName ++ "'s lifetime assists") $
          player^.pLifetime.psAssists `shouldBe` lt

        it ("should increment " ++ pName ++ "'s game assists") $
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
  players <- runIO $ replicateM 5 makePlayer
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
      .  (selectedPlayer  .~ pid)

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
          ps'^.progMode.gameStateL.selectedPlayer `shouldBe` Nothing)

    --  index,   bob lt, bob ytd, bob game, joe lt, joe ytd, joe game
    [ ( Just 0,  6,      5,       4,        6,      5,       0        )
    , ( Just 1,  4,      3,       2,        8,      7,       2        )
    , ( Just 2,  4,      3,       2,        6,      5,       0        )
    , ( Nothing, 4,      3,       2,        6,      5,       0        )
    ]

makePlayer :: IO Player
makePlayer = Player
  <$> makeNum
  <*> makeName
  <*> makeName
  <*> makePlayerStats
  <*> makePlayerStats

makeGoalie :: IO Goalie
makeGoalie = Goalie
  <$> makeNum
  <*> makeName
  <*> makeGoalieStats
  <*> makeGoalieStats

makePlayerStats :: IO PlayerStats
makePlayerStats = PlayerStats
  <$> makeNum
  <*> makeNum
  <*> makeNum

makeGoalieStats :: IO GoalieStats
makeGoalieStats = GoalieStats
  <$> makeNum
  <*> makeNum
  <*> makeNum
  <*> makeNum
  <*> makeNum
  <*> makeNum
  <*> makeNum

makeNum :: IO Int
makeNum = randomRIO (1, 10)

makeName :: IO String
makeName = replicateM 10 $ randomRIO ('A', 'Z')

backHomeSpec :: Spec
backHomeSpec = describe "backHome" $ do
  let
    input = newProgState
      & progMode.gameStateL .~ newGameState
      & inputBuffer         .~ "foo"
      & scrollOffset        .~ 123
    result = backHome input

  it "should set the program mode back to MainMenu" $
    result^.progMode `shouldSatisfy` \case
      MainMenu -> True
      _        -> False

  it "should clear the input buffer" $
    result^.inputBuffer `shouldBe` ""

  it "should reset the scroll offset" $
    result^.scrollOffset `shouldBe` 0
