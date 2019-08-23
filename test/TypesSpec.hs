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

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module TypesSpec (spec) where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Lens.Micro ((&), (^.), (.~), (?~))
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Text.RawString.QQ (r)
import Mtlstats.Types

import qualified Types.MenuSpec as Menu

spec :: Spec
spec = describe "Mtlstats.Types" $ do
  playerSpec
  pPointsSpec
  goalieSpec
  databaseSpec
  gameTypeLSpec
  homeScoreLSpec
  awayScoreLSpec
  teamScoreSpec
  Menu.spec

playerSpec :: Spec
playerSpec = describe "Player" $ do

  describe "decode" $
    it "should decode" $
      decode playerJSON `shouldBe` Just player

  describe "encode" $
    it "should encode" $
      decode (encode player) `shouldBe` Just player

pPointsSpec :: Spec
pPointsSpec = describe "pPoints" $ mapM_
  (\(goals, assists, points) -> let
    desc = "goals: " ++ show goals ++
      ", assists: " ++ show assists
    stats = newPlayerStats &
      psGoals   .~ goals &
      psAssists .~ assists
    in context desc $
      it ("should be " ++ show points) $
        pPoints stats `shouldBe` points)
  --  goals, assists, points
  [ ( 0,     0,       0      )
  , ( 1,     0,       1      )
  , ( 0,     1,       1      )
  , ( 2,     3,       5      )
  ]

goalieSpec :: Spec
goalieSpec = describe "Goalie" $ do

  describe "decode" $
    it "should decode" $
      decode goalieJSON `shouldBe` Just goalie

  describe "encode" $
    it "should encode" $
      decode (encode goalie) `shouldBe` Just goalie

databaseSpec :: Spec
databaseSpec = describe "Database" $ do

  describe "decode" $
    it "should decode" $
      decode dbJSON `shouldBe` Just db

  describe "encode" $
    it "should encode" $
      decode (encode db) `shouldBe` Just db

gameTypeLSpec :: Spec
gameTypeLSpec = describe "gameTypeL" $ do

  context "getter" $ do

    context "unexpected mode" $
      it "should return Nothing" $
        MainMenu ^. gameTypeL `shouldBe` Nothing

    mapM_
      (\t -> context (show t) $
        it ("should return " ++ show t) $ let
          gs = newGameState & gameType ?~ t
          m  = NewGame gs
          in m ^. gameTypeL `shouldBe` Just t)
      [HomeGame, AwayGame]

  context "setter" $ do

    context "unexpected mode" $
      mapM_
        (\t -> context (show t) $
          it ("should set to " ++ show t) $ let
            m = MainMenu & gameTypeL ?~ t
            in m ^. gameTypeL `shouldBe` Just t)
        [HomeGame, AwayGame]

    context "expected mode" $
      mapM_
        (\t -> context (show t) $
          it ("should set to " ++ show t) $ let
            m = NewGame newGameState & gameTypeL ?~ t
            in m ^. gameTypeL `shouldBe` Just t)
        [HomeGame, AwayGame]

homeScoreLSpec = describe "homeScoreL" $ do

  context "getter" $ do

    context "unexpected mode" $
      it "should return Nothing" $
        MainMenu ^. homeScoreL `shouldBe` Nothing

    context "expected mode" $
      it "should return 0" $ let
        gs = newGameState & homeScore ?~ 0
        m  = NewGame gs
        in m ^. homeScoreL `shouldBe` Just 0

  context "setter" $ do

    context "unexpected mode" $
      it "should set home score" $ let
        m = MainMenu & homeScoreL ?~ 0
        in m ^. homeScoreL `shouldBe` Just 0

    context "expected mode" $
      it "should set home score" $ let
        m = NewGame newGameState & homeScoreL ?~ 0
        in m ^. homeScoreL `shouldBe` Just 0

awayScoreLSpec :: Spec
awayScoreLSpec = describe "awayScoreL" $ do

  context "getter" $ do

    context "unexpected mode" $
      it "should return Nothing" $
        MainMenu ^. awayScoreL `shouldBe` Nothing

    context "expected mode" $
      it "should return 0" $ let
        gs = newGameState & awayScore ?~ 0
        m  = NewGame gs
        in m ^. awayScoreL `shouldBe` Just 0

  context "setter" $ do

    context "unexpected mode" $
      it "should set the away score" $ let
        m = MainMenu & awayScoreL ?~ 0
        in m ^. awayScoreL `shouldBe` Just 0

    context "expected mode" $
      it "should set the away score" $ let
        m = NewGame newGameState & awayScoreL ?~ 0
        in m ^. awayScoreL `shouldBe` Just 0

teamScoreSpec :: Spec
teamScoreSpec = describe "teamScore" $ do
  let
    m t = NewGame $ newGameState
      & gameType  ?~ t
      & homeScore ?~ 1
      & awayScore ?~ 2
    s t = newProgState
      & progMode .~ m t

  context "unexpected state" $
    it "should return Nothing" $
      teamScore newProgState `shouldBe` Nothing

  context "HomeGame" $
    it "should return 1" $
      teamScore (s HomeGame) `shouldBe` Just 1

  context "AwayGame" $
    it "should return 2" $
      teamScore (s AwayGame) `shouldBe` Just 2

player :: Player
player = newPlayer 1 "Joe" "centre"
  & pYtd . psGoals        .~ 2
  & pYtd . psAssists      .~ 3
  & pYtd . psPMin         .~ 4
  & pLifetime . psGoals   .~ 5
  & pLifetime . psAssists .~ 6
  & pLifetime . psPMin    .~ 7

goalie :: Goalie
goalie = newGoalie 1 "Joe"
  & gYtd . gsGames             .~ 2
  & gYtd . gsMinsPlayed        .~ 3
  & gYtd . gsGoalsAllowed      .~ 4
  & gYtd . gsGoalsAgainst      .~ 5
  & gYtd . gsWins              .~ 6
  & gYtd . gsLosses            .~ 7
  & gYtd . gsTies              .~ 8
  & gLifetime . gsGames        .~ 9
  & gLifetime . gsMinsPlayed   .~ 10
  & gLifetime . gsGoalsAllowed .~ 11
  & gLifetime . gsGoalsAgainst .~ 12
  & gLifetime . gsWins         .~ 13
  & gLifetime . gsLosses       .~ 14
  & gLifetime . gsTies         .~ 15

db :: Database
db = newDatabase
  & dbPlayers .~ [player]
  & dbGoalies .~ [goalie]
  & dbGames   .~ 1

playerJSON :: ByteString
playerJSON = [r|
  { "number": 1
  , "name": "Joe"
  , "position": "centre"
  , "ytd":
    { "goals": 2
    , "assists": 3
    , "penalty_mins": 4
    }
  , "lifetime":
    { "goals": 5
    , "assists": 6
    , "penalty_mins": 7
    }
  }|]

goalieJSON :: ByteString
goalieJSON = [r|
  { "number": 1
  , "name": "Joe"
  , "ytd":
    { "games": 2
    , "mins_played": 3
    , "goals_allowed": 4
    , "goals_against": 5
    , "wins": 6
    , "losses": 7
    , "ties": 8
    }
  , "lifetime":
    { "games": 9
    , "mins_played": 10
    , "goals_allowed": 11
    , "goals_against": 12
    , "wins": 13
    , "losses": 14
    , "ties": 15
    }
  }|]

dbJSON :: ByteString
dbJSON = [r|
  { "players":
    [ |] <> playerJSON <> [r| ]
  , "goalies":
    [ |] <> goalieJSON <> [r| ]
  , "games": 1
  }|]
