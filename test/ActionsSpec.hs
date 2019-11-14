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
import Lens.Micro ((^.), (&), (.~), (?~))
import Test.Hspec
  ( Spec
  , context
  , describe
  , it
  , shouldBe
  , shouldNotBe
  , shouldSatisfy
  )

import Mtlstats.Actions
import Mtlstats.Types

import qualified Actions.EditGoalieSpec as EditGoalie
import qualified Actions.NewGameSpec as NewGame
import qualified TypesSpec as TS

spec :: Spec
spec = describe "Mtlstats.Actions" $ do
  startNewSeasonSpec
  startNewGameSpec
  resetYtdSpec
  addCharSpec
  removeCharSpec
  createPlayerSpec
  createGoalieSpec
  editPlayerSpec
  editGoalieSpec
  addPlayerSpec
  addGoalieSpec
  resetCreatePlayerStateSpec
  resetCreateGoalieStateSpec
  backHomeSpec
  scrollUpSpec
  scrollDownSpec
  NewGame.spec
  EditGoalie.spec

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
    ps <- replicateM 2 TS.makePlayer
    gs <- replicateM 2 TS.makeGoalie
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
        ytd ^. gsWins         `shouldBe`    0
        ytd ^. gsLosses       `shouldBe`    0
        ytd ^. gsTies         `shouldBe`    0
        lt ^. gsGames         `shouldNotBe` 0
        lt ^. gsMinsPlayed    `shouldNotBe` 0
        lt ^. gsGoalsAllowed  `shouldNotBe` 0
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

createPlayerSpec :: Spec
createPlayerSpec = describe "createPlayer" $
  it "should change the mode appropriately" $ let
    s = createPlayer newProgState
    in show (s^.progMode) `shouldBe` "CreatePlayer"

createGoalieSpec :: Spec
createGoalieSpec = describe "createGoalie" $
  it "should change the mode appropriately" $ let
    s = createGoalie newProgState
    in show (s^.progMode) `shouldBe` "CreateGoalie"

editPlayerSpec :: Spec
editPlayerSpec = describe "editPlayer" $
  it "should change the mode appropriately" $ let
    s = editPlayer newProgState
    in show (s^.progMode) `shouldBe` "EditPlayer"

editGoalieSpec :: Spec
editGoalieSpec = describe "editGoalie" $
  it "should change the mode appropriately" $ let
    s = editGoalie newProgState
    in show (s^.progMode) `shouldBe` "EditGoalie"

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

addGoalieSpec :: Spec
addGoalieSpec = describe "addGoalie" $ do
  let
    g1 = newGoalie 2 "Joe"
    g2 = newGoalie 3 "Bob"
    db = newDatabase
      & dbGoalies .~ [g1]
    s pm = newProgState
      & database .~ db
      & progMode .~ pm

  context "data available" $
    it "should create the goalie" $ let
      s' = addGoalie $ s $ CreateGoalie $ newCreateGoalieState
        & cgsNumber ?~ 3
        & cgsName   .~ "Bob"
      in s'^.database.dbGoalies `shouldBe` [g1, g2]

  context "data unavailable" $
    it "should not create the goalie" $ let
      s' = addGoalie $ s MainMenu
      in s'^.database.dbGoalies `shouldBe` [g1]

resetCreatePlayerStateSpec :: Spec
resetCreatePlayerStateSpec = describe "resetCreatePlayerState" $ let
  cps = newCreatePlayerState
    & cpsNumber   ?~ 1
    & cpsName     .~ "Joe"
    & cpsPosition .~ "centre"
  ps  = resetCreatePlayerState $
    newProgState & progMode.createPlayerStateL .~ cps
  in TS.compareTest (ps^.progMode.createPlayerStateL) newCreatePlayerState

resetCreateGoalieStateSpec :: Spec
resetCreateGoalieStateSpec = describe "resetCreateGoalieState" $ let
  cgs = newCreateGoalieState
    & cgsNumber ?~ 1
    & cgsName   .~ "Joe"
  ps = resetCreateGoalieState $
    newProgState & progMode.createGoalieStateL .~ cgs
  in TS.compareTest (ps^.progMode.createGoalieStateL) newCreateGoalieState

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

scrollUpSpec :: Spec
scrollUpSpec = describe "scrollUp" $ do

  context "scrolled down" $
    it "should decrease the scroll offset by one" $ let
      ps  = newProgState & scrollOffset .~ 10
      ps' = scrollUp ps
      in ps'^.scrollOffset `shouldBe` 9

  context "at top" $
    it "should keep the scroll offset at zero" $ let
      ps = scrollUp newProgState
      in ps^.scrollOffset `shouldBe` 0

  context "above top" $
    it "should return the scroll offset to zero" $ let
      ps  = newProgState & scrollOffset .~ (-10)
      ps' = scrollUp ps
      in ps'^.scrollOffset `shouldBe` 0

scrollDownSpec :: Spec
scrollDownSpec = describe "scrollDown" $
  it "should increase the scroll offset" $ let
    ps  = newProgState & scrollOffset .~ 10
    ps' = scrollDown ps
    in ps'^.scrollOffset `shouldBe` 11
