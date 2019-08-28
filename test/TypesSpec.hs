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

{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module TypesSpec (spec) where

import Data.Aeson (FromJSON, ToJSON, decode, encode, toJSON)
import Data.Aeson.Types (Value (Object))
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HM
import Lens.Micro (Lens', (&), (^.), (.~), (?~))
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
  otherScoreSpec
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
gameTypeLSpec = describe "gameTypeL" $ lensSpec gameTypeL
  [ ( MainMenu,   Nothing       )
  , ( m HomeGame, Just HomeGame )
  , ( m AwayGame, Just AwayGame )
  ]
  [ ( MainMenu,   Just HomeGame )
  , ( MainMenu,   Just AwayGame )
  , ( m HomeGame, Just AwayGame )
  , ( m AwayGame, Just HomeGame )
  , ( m HomeGame, Nothing       )
  ]
  where m t = NewGame $ newGameState & gameType ?~ t

otherTeamLSpec :: Spec
otherTeamLSpec = describe "otherTeamL" $ lensSpec otherTeamL
  [ ( MainMenu, ""    )
  , ( m "foo",  "foo" )
  ]
  [ ( MainMenu, "foo" )
  , ( m "foo",  "bar" )
  , ( m "foo",  ""    )
  ]
  where m t = NewGame $ newGameState & otherTeam .~ t

homeScoreLSpec :: Spec
homeScoreLSpec = describe "homeScoreL" $ lensSpec homeScoreL
  [ ( MainMenu, Nothing )
  , ( m 1,      Just 1  )
  ]
  [ ( MainMenu, Just 1  )
  , ( m 1,      Just 2  )
  , ( m 1,      Nothing )
  ]
  where m s = NewGame $ newGameState & homeScore ?~ s

awayScoreLSpec :: Spec
awayScoreLSpec = describe "awayScoreL" $ lensSpec awayScoreL
  [ ( MainMenu, Nothing )
  , ( m 1,      Just 1  )
  ]
  [ ( MainMenu, Just 1  )
  , ( m 1,      Just 2  )
  , ( m 1,      Nothing )
  ]
  where m s = NewGame $ newGameState & awayScore ?~ s

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

otherScoreSpec :: Spec
otherScoreSpec = describe "otherScore" $ do
  let
    s t = newGameState
      & gameType  ?~ t
      & homeScore ?~ 1
      & awayScore ?~ 2

  context "unknown game type" $
    it "should return Nothing" $
      otherScore newGameState `shouldBe` Nothing

  context "HomeGame" $
    it "should return 2" $
      otherScore (s HomeGame) `shouldBe` Just 2

  context "AwayGame" $
    it "should return 1" $
      otherScore (s AwayGame) `shouldBe` Just 1

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

lensSpec
  :: (Eq a, Show s, Show a)
  => Lens' s a
  -> [(s, a)]
  -> [(s, a)]
  -> Spec
lensSpec l gs ss = do

  context "getters" $ mapM_
    (\(s, x) -> context (show s) $
      it ("should be " ++ show x) $
        s ^. l `shouldBe` x)
    gs

  context "setters" $ mapM_
    (\(s, x) -> context (show s) $
      it ("should set to " ++ show x) $
        (s & l .~ x) ^. l `shouldBe` x)
    ss

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
