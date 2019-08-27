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

{-# LANGUAGE OverloadedStrings #-}

module TypesSpec (spec) where

import Data.Aeson (FromJSON, ToJSON, decode, encode, toJSON)
import Data.Aeson.Types (Value (Object))
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HM
import Lens.Micro ((&), (^.), (.~), (?~))
import Test.Hspec (Spec, context, describe, it, shouldBe)

import Mtlstats.Types

import qualified Types.MenuSpec as Menu

spec :: Spec
spec = describe "Mtlstats.Types" $ do
  playerSpec
  goalieSpec
  gameStatsSpec
  databaseSpec
  pPointsSpec
  gameTypeLSpec
  otherTeamLSpec
  homeScoreLSpec
  awayScoreLSpec
  teamScoreSpec
  Menu.spec

playerSpec :: Spec
playerSpec = describe "Player" $ jsonSpec player playerJSON

goalieSpec :: Spec
goalieSpec = describe "Goalie" $ jsonSpec goalie goalieJSON

gameStatsSpec :: Spec
gameStatsSpec = describe "GameStats" $
  jsonSpec (gameStats 1) (gameStatsJSON 1)

databaseSpec :: Spec
databaseSpec = describe "Database" $ jsonSpec db dbJSON

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

otherTeamLSpec :: Spec
otherTeamLSpec = describe "otherTeamL" $ do

  context "getter" $ do

    context "unexpected mode" $
      it "should return an empty string" $
        MainMenu ^. otherTeamL `shouldBe` ""

    context "expected mode" $
      it "should return \"foo\"" $ let
        m = NewGame $ newGameState & otherTeam .~ "foo"
        in m ^. otherTeamL `shouldBe` "foo"

  context "setter" $ do

    context "unexpected mode" $
      it "should set the value" $ let
        m = MainMenu & otherTeamL .~ "foo"
        in m ^. otherTeamL `shouldBe` "foo"

    context "expected mode" $
      it "should set the value" $ let
        m = NewGame newGameState & otherTeamL .~ "foo"
        in m ^. otherTeamL `shouldBe` "foo"

homeScoreLSpec :: Spec
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
    s t = newGameState
      & gameType  ?~ t
      & homeScore ?~ 1
      & awayScore ?~ 2

  context "unknown game type" $
    it "should return Nothing" $
      teamScore newGameState `shouldBe` Nothing

  context "HomeGame" $
    it "should return 1" $
      teamScore (s HomeGame) `shouldBe` Just 1

  context "AwayGame" $
    it "should return 2" $
      teamScore (s AwayGame) `shouldBe` Just 2

jsonSpec
  :: (Eq a, Show a, FromJSON a, ToJSON a)
  => a
  -> Value
  -> Spec
jsonSpec x j = do

  describe "decode" $
    it "should decode" $
      decode (encode j) `shouldBe` Just x

  describe "toJSON" $
    it "should encode" $
      decode (encode $ toJSON x) `shouldBe` Just x

  describe "toEncoding" $
    it "should encode" $
      decode (encode x) `shouldBe` Just x

player :: Player
player = newPlayer 1 "Joe" "centre"
  & pYtd      .~ playerStats 1
  & pLifetime .~ playerStats 2

playerJSON :: Value
playerJSON = Object $ HM.fromList
  [ ( "number",   toJSON (1 :: Int)           )
  , ( "name",     toJSON ("Joe" :: String)    )
  , ( "position", toJSON ("centre" :: String) )
  , ( "ytd",      playerStatsJSON 1           )
  , ( "lifetime", playerStatsJSON 2           )
  ]

playerStats :: Int -> PlayerStats
playerStats n = newPlayerStats
  & psGoals   .~ n
  & psAssists .~ n + 1
  & psPMin    .~ n + 2

playerStatsJSON :: Int -> Value
playerStatsJSON n = Object $ HM.fromList
  [ ( "goals",        toJSON n       )
  , ( "assists",      toJSON $ n + 1 )
  , ( "penalty_mins", toJSON $ n + 2 )
  ]

goalie :: Goalie
goalie = newGoalie 1 "Joe"
  & gYtd      .~ goalieStats 1
  & gLifetime .~ goalieStats 2

goalieJSON :: Value
goalieJSON = Object $ HM.fromList
  [ ( "number",   toJSON (1 :: Int)         )
  , ( "name",     toJSON ("Joe" :: String ) )
  , ( "ytd",      goalieStatsJSON 1         )
  , ( "lifetime", goalieStatsJSON 2         )
  ]

goalieStats :: Int -> GoalieStats
goalieStats n = newGoalieStats
  & gsGames        .~ n
  & gsMinsPlayed   .~ n + 1
  & gsGoalsAllowed .~ n + 2
  & gsGoalsAgainst .~ n + 3
  & gsWins         .~ n + 4
  & gsLosses       .~ n + 5
  & gsTies         .~ n + 6

goalieStatsJSON :: Int -> Value
goalieStatsJSON n = Object $ HM.fromList
  [ ( "games",         toJSON n       )
  , ( "mins_played",   toJSON $ n + 1 )
  , ( "goals_allowed", toJSON $ n + 2 )
  , ( "goals_against", toJSON $ n + 3 )
  , ( "wins",          toJSON $ n + 4 )
  , ( "losses",        toJSON $ n + 5 )
  , ( "ties",          toJSON $ n + 6 )
  ]

gameStats :: Int -> GameStats
gameStats n = GameStats
  { _gmsWins     = n
  , _gmsLosses   = n + 1
  , _gmsOvertime = n + 2
  }

gameStatsJSON :: Int -> Value
gameStatsJSON n = Object $ HM.fromList
  [ ( "wins",     toJSON n       )
  , ( "losses",   toJSON $ n + 1 )
  , ( "overtime", toJSON $ n + 2 )
  ]

db :: Database
db = newDatabase
  & dbPlayers       .~ [player]
  & dbGoalies       .~ [goalie]
  & dbGames         .~ 1
  & dbHomeGameStats .~ gameStats 1
  & dbAwayGameStats .~ gameStats 2

dbJSON :: Value
dbJSON = Object $ HM.fromList
  [ ( "players",         toJSON [playerJSON] )
  , ( "goalies",         toJSON [goalieJSON] )
  , ( "games",           toJSON (1 :: Int)   )
  , ( "home_game_stats", gameStatsJSON 1     )
  , ( "away_game_stats", gameStatsJSON 2     )
  ]
