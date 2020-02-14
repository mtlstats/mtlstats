{-

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

module ActionsSpec (spec) where

import Control.Monad (replicateM)
import Lens.Micro ((^.), (&), (.~), (?~), (%~))
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

import qualified Actions.NewGameSpec as NewGame
import qualified Actions.EditStandingsSpec as EditStandings
import qualified TypesSpec as TS

spec :: Spec
spec = describe "Mtlstats.Actions" $ do
  startNewSeasonSpec
  startNewGameSpec
  resetYtdSpec
  clearRookiesSpec
  resetStandingsSpec
  addCharSpec
  removeCharSpec
  createPlayerSpec
  createGoalieSpec
  editSpec
  editPlayerSpec
  editSelectedPlayerSpec
  editGoalieSpec
  editSelectedGoalieSpec
  addPlayerSpec
  addGoalieSpec
  resetCreatePlayerStateSpec
  resetCreateGoalieStateSpec
  backHomeSpec
  scrollUpSpec
  scrollDownSpec
  NewGame.spec
  EditStandings.spec

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

clearRookiesSpec :: Spec
clearRookiesSpec = describe "clearRookies" $ do
  let

    players =
      [ newPlayer 1 "Joe" "centre" & pRookie .~ True
      , newPlayer 2 "Bob" "centre" & pRookie .~ False
      ]

    goalies =
      [ newGoalie 3 "Bill" & gRookie .~ True
      , newGoalie 4 "Doug" & gRookie .~ False
      ]

    ps = newProgState
      & database
        %~ (dbPlayers .~ players)
        .  (dbGoalies .~ goalies)

    ps' = clearRookies ps

  context "Players" $ mapM_
    (\p -> let
      name  = p^.pName
      rFlag = p^.pRookie
      in context name $
        it "should not be a rookie" $
          rFlag `shouldBe` False)
    (ps'^.database.dbPlayers)

  context "Goalies" $ mapM_
    (\g -> let
      name  = g^.gName
      rFlag = g^.gRookie
      in context name $
        it "should not be a rookie" $
          rFlag `shouldBe` False)
    (ps'^.database.dbGoalies)

resetStandingsSpec :: Spec
resetStandingsSpec = describe "resetStandings" $ do
  let
    home = GameStats
      { _gmsWins         = 1
      , _gmsLosses       = 2
      , _gmsOvertime     = 3
      , _gmsGoalsFor     = 4
      , _gmsGoalsAgainst = 5
      }

    away = GameStats
      { _gmsWins         = 6
      , _gmsLosses       = 7
      , _gmsOvertime     = 8
      , _gmsGoalsFor     = 9
      , _gmsGoalsAgainst = 10
      }

    db = newDatabase
      & dbHomeGameStats .~ home
      & dbAwayGameStats .~ away

    ps = newProgState
      & database .~ db
      & resetStandings

  context "home standings" $
    it "should be reset" $
      ps^.database.dbHomeGameStats `shouldBe` newGameStats

  context "away standings" $
    it "should be reset" $
      ps^.database.dbAwayGameStats `shouldBe` newGameStats

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

editSpec :: Spec
editSpec = describe "edit" $
  it "should change the mode to EditMenu" $ let
    ps = edit newProgState
    in show (ps^.progMode) `shouldBe` "EditMenu"

editPlayerSpec :: Spec
editPlayerSpec = describe "editPlayer" $
  it "should change the mode appropriately" $ let
    s = editPlayer newProgState
    in show (s^.progMode) `shouldBe` "EditPlayer"

editSelectedPlayerSpec :: Spec
editSelectedPlayerSpec = describe "editSelectedPlayer" $ mapM_
  (\(label, pState, expected) -> context label $
    it "should edit the players appropriately" $ let
      pState'  = editSelectedPlayer (pName .~ "foo") pState
      players' = pState'^.database.dbPlayers
      in players' `shouldBe` expected)

  --  label,           initial state,         expected
  [ ( "wrong mode",    baseState,             players  )
  , ( "not selected",  changePlayer Nothing,  players  )
  , ( "player 0",      changePlayer $ Just 0, changed0 )
  , ( "player 1",      changePlayer $ Just 1, changed1 )
  , ( "out of bounds", changePlayer $ Just 2, players  )
  ]

  where
    baseState      = newProgState & database.dbPlayers .~ players
    changePlayer n = baseState
      & (progMode.editPlayerStateL.epsSelectedPlayer .~ n)
    players        = [ player 0,  player 1  ]
    changed0       = [ player' 0, player 1  ]
    changed1       = [ player 0,  player' 1 ]
    player n       = newPlayer n ("Player " ++ show n) "pos"
    player' n      = newPlayer n "foo" "pos"

editGoalieSpec :: Spec
editGoalieSpec = describe "editGoalie" $
  it "should change the mode appropriately" $ let
    s = editGoalie newProgState
    in show (s^.progMode) `shouldBe` "EditGoalie"

editSelectedGoalieSpec :: Spec
editSelectedGoalieSpec = describe "editSelectedGoalie" $ mapM_
  (\(label, pState, expected) -> context label $
    it "should edit the goalies appropriately" $ let
      pState'  = editSelectedGoalie (gName .~ "foo") pState
      goalies' = pState'^.database.dbGoalies
      in goalies' `shouldBe` expected)

  --  label,           initial state,         expected
  [ ( "wrong mode",    baseState,             goalies  )
  , ( "not selected",  changeGoalie Nothing,  goalies  )
  , ( "goalie 0",      changeGoalie $ Just 0, changed0 )
  , ( "goalie 1",      changeGoalie $ Just 1, changed1 )
  , ( "out of bounds", changeGoalie $ Just 2, goalies  )
  ]

  where
    baseState      = newProgState & database.dbGoalies .~ goalies
    changeGoalie n = baseState
      & (progMode.editGoalieStateL.egsSelectedGoalie .~ n)
    goalies        = [ goalie 0,  goalie 1  ]
    changed0       = [ goalie' 0, goalie 1  ]
    changed1       = [ goalie 0,  goalie' 1 ]
    goalie n       = newGoalie n ("Player " ++ show n)
    goalie' n      = newGoalie n "foo"

addPlayerSpec :: Spec
addPlayerSpec = describe "addPlayer" $ mapM_
  (\(label, expectation, pm, players) -> context label $
    it expectation $ let
      ps = newProgState
        & progMode           .~ pm
        & database.dbPlayers .~ [joe]
      ps' = addPlayer ps
      in ps'^.database.dbPlayers `shouldBe` players)

  --  label,                 expectation, progMode,  players
  [ ( "wrong mode",          failure,     MainMenu,  [joe]          )
  , ( "missing number",      failure,     noNum,     [joe]          )
  , ( "missing rookie flag", failure,     noRookie,  [joe]          )
  , ( "missing active flag", failure,     noActive,  [joe]          )
  , ( "rookie",              success,     mkRookie,  [joe, rookie]  )
  , ( "retired",             success,     mkRetired, [joe, retired] )
  , ( "normal player",       success,     mkNormal,  [joe, normal]  )
  ]

  where
    failure   = "should not create the player"
    success   = "should create the player"
    noNum     = mkpm Nothing  (Just False) (Just True)
    noRookie  = mkpm (Just 3) Nothing      (Just True)
    noActive  = mkpm (Just 3) (Just False) Nothing
    mkRookie  = mkpm (Just 3) (Just True)  (Just True)
    mkRetired = mkpm (Just 3) (Just False) (Just False)
    mkNormal  = mkpm (Just 3) (Just False) (Just True)
    joe       = newPlayer 2 "Joe" "centre"
    rookie    = player True  True
    retired   = player False False
    normal    = player False True

    player r a = newPlayer 3 "Bob" "defense"
      & pRookie .~ r
      & pActive .~ a

    mkpm n r a = CreatePlayer $ newCreatePlayerState
      & cpsNumber     .~ n
      & cpsName       .~ "Bob"
      & cpsPosition   .~ "defense"
      & cpsRookieFlag .~ r
      & cpsActiveFlag .~ a

addGoalieSpec :: Spec
addGoalieSpec = describe "addGoalie" $ mapM_
  (\(label, expectation, pm, goalies) -> context label $
    it expectation $ let
      ps = newProgState
        & progMode           .~ pm
        & database.dbGoalies .~ [joe]
      ps' = addGoalie ps
      in ps'^.database.dbGoalies `shouldBe` goalies)

  --  label,            expectation, progMode,  expected goalies
  [ ( "wrong mode",     failure,     MainMenu,  [joe]            )
  , ( "no number",      failure,     noNum,     [joe]            )
  , ( "no rookie flag", failure,     noRookie,  [joe]            )
  , ( "no active flag", failure,     noActive,  [joe]            )
  , ( "rookie",         success,     mkRookie,  [joe, rookie]    )
  , ( "retired",        success,     mkRetired, [joe, retired]   )
  , ( "normal goalie",  success,     mkNormal,  [joe, normal]    )
  ]

  where
    failure   = "should not create the goalie"
    success   = "should create the goalie"
    noNum     = cgs Nothing  (Just False) (Just True)
    noRookie  = cgs (Just 3) Nothing      (Just True)
    noActive  = cgs (Just 3) (Just False) Nothing
    mkRookie  = cgs (Just 3) (Just True)  (Just True)
    mkRetired = cgs (Just 3) (Just False) (Just False)
    mkNormal  = cgs (Just 3) (Just False) (Just True)
    joe       = newGoalie 2 "Joe"
    rookie    = goalie True  True
    retired   = goalie False False
    normal    = goalie False True

    goalie r a = newGoalie 3 "Bob"
      & gRookie .~ r
      & gActive .~ a

    cgs n r a = CreateGoalie $ newCreateGoalieState
      & cgsNumber     .~ n
      & cgsName       .~ "Bob"
      & cgsRookieFlag .~ r
      & cgsActiveFlag .~ a

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
