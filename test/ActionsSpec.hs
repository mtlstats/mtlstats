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

module ActionsSpec (spec) where

import Control.Monad (replicateM)
import Lens.Micro ((^.), (&), (.~), (?~), (%~))
import System.Random (randomRIO)
import Test.Hspec (Spec, context, describe, it, shouldBe, shouldNotBe)

import Mtlstats.Actions
import Mtlstats.Types

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

startNewSeasonSpec :: Spec
startNewSeasonSpec = describe "startNewSeason" $ do
  let
    s = newProgState
      & database . dbGames .~ 1
      & startNewSeason

  it "should set the progState to NewSeason" $
    s ^. progMode `shouldBe` NewSeason

  it "should set the number of games to 0" $
    s ^. database . dbGames `shouldBe` 0

startNewGameSpec :: Spec
startNewGameSpec = describe "startNewGame" $ do
  let s = startNewGame newProgState

  it "should increment the number of games" $
    s ^. database . dbGames `shouldBe` 1

  it "should set the mode to NewGame" $
    s ^. progMode `shouldBe` NewGame newGameState

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
      & gmsWins     .~ 1
      & gmsLosses   .~ 1
      & gmsOvertime .~ 1

    s t h a o = newProgState
      & progMode.gameStateL
        %~ (gameType     .~ t)
        .  (homeScore    .~ h)
        .  (awayScore    .~ a)
        .  (overtimeFlag .~ o)
      & database
        %~ (dbHomeGameStats .~ baseStats)
        .  (dbAwayGameStats .~ baseStats)

    db hw hl ho aw al ao = newDatabase
      & dbHomeGameStats
        %~ (gmsWins     .~ hw)
        .  (gmsLosses   .~ hl)
        .  (gmsOvertime .~ ho)
      & dbAwayGameStats
        %~ (gmsWins     .~ aw)
        .  (gmsLosses   .~ al)
        .  (gmsOvertime .~ ao)

  context "home win" $
    it "should record a home win" $ let
      s'  = s (Just HomeGame) (Just 2) (Just 1) (Just False)
      db' = updateGameStats s' ^. database
      in db' `shouldBe` db 2 1 1 1 1 1

  context "home loss" $
    it "should record a home loss" $ let
      s'  = s (Just HomeGame) (Just 1) (Just 2) (Just False)
      db' = updateGameStats s' ^. database
      in db' `shouldBe` db 1 2 1 1 1 1

  context "home overtime loss" $
    it "should record a home overtime" $ let
      s'  = s (Just HomeGame) (Just 1) (Just 2) (Just True)
      db' = updateGameStats s' ^. database
      in db' `shouldBe` db 1 1 2 1 1 1

  context "away win" $
    it "should record an away win" $ let
      s'  = s (Just AwayGame) (Just 1) (Just 2) (Just False)
      db' = updateGameStats s' ^. database
      in db' `shouldBe` db 1 1 1 2 1 1

  context "away loss" $
    it "should record an away loss" $ let
      s'  = s (Just AwayGame) (Just 2) (Just 1) (Just False)
      db' = updateGameStats s' ^. database
      in db' `shouldBe` db 1 1 1 1 2 1

  context "away overtime loss" $
    it "should record an away overtime" $ let
      s'  = s (Just AwayGame) (Just 2) (Just 1) (Just True)
      db' = updateGameStats s' ^. database
      in db' `shouldBe` db 1 1 1 1 1 2

  context "missing game type" $
    it "should not change anything" $ let
      s' = s Nothing (Just 1) (Just 2) (Just True)
      in updateGameStats s' `shouldBe` s'

  context "missing home score" $
    it "should not change anything" $ let
      s' = s (Just HomeGame) Nothing (Just 1) (Just True)
      in updateGameStats s' `shouldBe` s'

  context "missing away score" $
    it "should not change anything" $ let
      s' = s (Just HomeGame) (Just 1) Nothing (Just True)
      in updateGameStats s' `shouldBe` s'

  context "missing overtime flag" $
    it "should not change anything" $ let
      s' = s (Just HomeGame) (Just 1) (Just 2) Nothing
      in updateGameStats s' `shouldBe` s'

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
    in s^.progMode `shouldBe` CreatePlayer newCreatePlayerState

addPlayerSpec :: Spec
addPlayerSpec = describe "addPlayer" $ do
  let
    p1 = newPlayer 1 "Joe" "centre"
    p2 = newPlayer 2 "Bob" "defense"
    db = newDatabase
      & dbPlayers .~ [p2]
    s pm = newProgState
      & progMode .~ pm
      & database .~ db

  context "data available" $
    it "should create the player" $ let
      s' = addPlayer $ s $ CreatePlayer $ newCreatePlayerState
        & cpsNumber   ?~ 1
        & cpsName     .~ "Joe"
        & cpsPosition .~ "centre"
      in s'^.database.dbPlayers `shouldBe` [p1, p2]

  context "data unavailable" $
    it "should not create the player" $ let
      s' = addPlayer $ s MainMenu
      in s'^.database.dbPlayers `shouldBe` [p2]

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
