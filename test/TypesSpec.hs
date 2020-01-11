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

{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module TypesSpec
  ( Comparable (..)
  , spec
  , makePlayer
  , makeGoalie
  , makePlayerStats
  , makeGoalieStats
  ) where

import Control.Monad (replicateM)
import Data.Aeson (FromJSON, ToJSON, decode, encode, toJSON)
import Data.Aeson.Types (Value (Object))
import qualified Data.HashMap.Strict as HM
import Data.Ratio ((%))
import Lens.Micro (Lens', (&), (^.), (.~), (?~))
import System.Random (randomIO, randomRIO)
import Test.Hspec (Spec, context, describe, it, shouldBe)

import Mtlstats.Config
import Mtlstats.Types

import qualified Types.MenuSpec as Menu

class Comparable a where
  compareTest :: a -> a -> Spec

spec :: Spec
spec = describe "Mtlstats.Types" $ do
  playerSpec
  goalieSpec
  gameStatsSpec
  databaseSpec
  gameStateLSpec
  createPlayerStateLSpec
  createGoalieStateLSpec
  editPlayerStateLSpec
  editGoalieStateLSpec
  teamScoreSpec
  otherScoreSpec
  homeTeamSpec
  awayTeamSpec
  gameWonSpec
  gameLostSpec
  gameTiedSpec
  unaccountedPointsSpec
  gmsGamesSpec
  gmsPointsSpec
  addGameStatsSpec
  playerSearchSpec
  playerSearchExactSpec
  modifyPlayerSpec
  playerSummarySpec
  playerIsActiveSpec
  psPointsSpec
  addPlayerStatsSpec
  goalieSearchSpec
  goalieSearchExactSpec
  goalieSummarySpec
  goalieIsActiveSpec
  addGoalieStatsSpec
  gsAverageSpec
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

gameStateLSpec :: Spec
gameStateLSpec = describe "gameStateL" $ lensSpec gameStateL
  -- getters
  [ ( "missing state", MainMenu,              newGameState )
  , ( "home game",     NewGame $ gs HomeGame, gs HomeGame  )
  , ( "away game",     NewGame $ gs AwayGame, gs AwayGame  )
  ]
  -- setters
  [ ( "set home",     MainMenu,              gs HomeGame  )
  , ( "home to away", NewGame $ gs HomeGame, gs AwayGame  )
  , ( "away to home", NewGame $ gs AwayGame, gs HomeGame  )
  , ( "clear home",   NewGame $ gs HomeGame, newGameState )
  , ( "clear away",   NewGame $ gs AwayGame, newGameState )
  ]
  where gs t = newGameState & gameType ?~ t

createPlayerStateLSpec :: Spec
createPlayerStateLSpec = describe "createPlayerStateL" $
  lensSpec createPlayerStateL
  -- getters
  [ ( "missing state", MainMenu,          newCreatePlayerState )
  , ( "with state",    CreatePlayer cps1, cps1                 )
  ]
  -- setters
  [ ( "missing state", MainMenu,          cps1                 )
  , ( "change state",  CreatePlayer cps1, cps2                 )
  , ( "clear state",   CreatePlayer cps1, newCreatePlayerState )
  ]
  where
    cps1 = newCreatePlayerState
      & cpsNumber    ?~ 1
      & cpsName      .~ "Joe"
      & cpsPosition  .~ "centre"
    cps2 = newCreatePlayerState
      & cpsNumber   ?~ 2
      & cpsName     .~ "Bob"
      & cpsPosition .~ "defense"

createGoalieStateLSpec :: Spec
createGoalieStateLSpec = describe "createGoalieStateL" $
  lensSpec createGoalieStateL
  -- getters
  [ ( "missing state", MainMenu,          newCreateGoalieState )
  , ( "with state",    CreateGoalie cgs1, cgs1                 )
  ]
  -- setters
  [ ( "set state",    MainMenu,          cgs1                 )
  , ( "change state", CreateGoalie cgs1, cgs2                 )
  , ( "clear state",  CreateGoalie cgs1, newCreateGoalieState )
  ]
  where
    cgs1 = newCreateGoalieState
      & cgsNumber    ?~ 1
      & cgsName      .~ "Joe"
    cgs2 = newCreateGoalieState
      & cgsNumber ?~ 2
      & cgsName   .~ "Bob"

editPlayerStateLSpec :: Spec
editPlayerStateLSpec = describe "editPlayerStateL" $
  lensSpec editPlayerStateL
  -- getters
  [ ( "missing state", MainMenu,        newEditPlayerState )
  , ( "withState",     EditPlayer eps1, eps1               )
  ]
  -- setters
  [ ( "set state",    MainMenu,        eps1               )
  , ( "change state", EditPlayer eps1, eps2               )
  , ( "clear state",  EditPlayer eps1, newEditPlayerState )
  ]
  where
    eps1 = newEditPlayerState
      & epsSelectedPlayer ?~ 1
    eps2 = newEditPlayerState
      & epsSelectedPlayer ?~ 2

editGoalieStateLSpec :: Spec
editGoalieStateLSpec = describe "editGoalieStateL" $
  lensSpec editGoalieStateL
  -- getters
  [ ( "missing state", MainMenu,        newEditGoalieState )
  , ( "with state",    EditGoalie egs1, egs1               )
  ]
  -- setters
  [ ( "set state",    MainMenu,        egs1               )
  , ( "change state", EditGoalie egs1, egs2               )
  , ( "clear state",  EditGoalie egs1, newEditGoalieState )
  ]
  where
    egs1 = newEditGoalieState
      & egsSelectedGoalie ?~ 1
    egs2 = newEditGoalieState
      & egsSelectedGoalie ?~ 2

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
  :: Comparable a
  => Lens' s a
  -> [(String, s, a)]
  -> [(String, s, a)]
  -> Spec
lensSpec lens getters setters = do

  context "getters" $ mapM_
    (\(label, s, x) -> context label $
      compareTest (s^.lens) x)
    getters

  context "setters" $ mapM_
    (\(label, s, x) -> context label $ let
      s' = s & lens .~ x
      in compareTest (s'^.lens) x)
    setters

player :: Player
player = newPlayer 1 "Joe" "centre"
  & pRookie   .~ False
  & pYtd      .~ playerStats 1
  & pLifetime .~ playerStats 2

playerJSON :: Value
playerJSON = Object $ HM.fromList
  [ ( "number",   toJSON (1 :: Int)           )
  , ( "name",     toJSON ("Joe" :: String)    )
  , ( "position", toJSON ("centre" :: String) )
  , ( "rookie",   toJSON False                )
  , ( "active",   toJSON True                 )
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
  & gRookie   .~ False
  & gYtd      .~ goalieStats 1
  & gLifetime .~ goalieStats 2

goalieJSON :: Value
goalieJSON = Object $ HM.fromList
  [ ( "number",   toJSON (1 :: Int)         )
  , ( "name",     toJSON ("Joe" :: String ) )
  , ( "rookie",   toJSON False              )
  , ( "active",   toJSON True               )
  , ( "ytd",      goalieStatsJSON 1         )
  , ( "lifetime", goalieStatsJSON 2         )
  ]

goalieStats :: Int -> GoalieStats
goalieStats n = newGoalieStats
  & gsGames        .~ n
  & gsMinsPlayed   .~ n + 1
  & gsGoalsAllowed .~ n + 2
  & gsShutouts     .~ n + 3
  & gsWins         .~ n + 4
  & gsLosses       .~ n + 5
  & gsTies         .~ n + 6

goalieStatsJSON :: Int -> Value
goalieStatsJSON n = Object $ HM.fromList
  [ ( "games",         toJSON n       )
  , ( "mins_played",   toJSON $ n + 1 )
  , ( "goals_allowed", toJSON $ n + 2 )
  , ( "shutouts",      toJSON $ n + 3 )
  , ( "wins",          toJSON $ n + 4 )
  , ( "losses",        toJSON $ n + 5 )
  , ( "ties",          toJSON $ n + 6 )
  ]

gameStats :: Int -> GameStats
gameStats n = GameStats
  { _gmsWins         = n
  , _gmsLosses       = n + 1
  , _gmsOvertime     = n + 2
  , _gmsGoalsFor     = n + 3
  , _gmsGoalsAgainst = n + 4
  }

gameStatsJSON :: Int -> Value
gameStatsJSON n = Object $ HM.fromList
  [ ( "wins",          toJSON n       )
  , ( "losses",        toJSON $ n + 1 )
  , ( "overtime",      toJSON $ n + 2 )
  , ( "goals_for",     toJSON $ n + 3 )
  , ( "goals_against", toJSON $ n + 4 )
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

homeTeamSpec :: Spec
homeTeamSpec = describe "homeTeam" $ do
  let
    gs gt = newGameState
      & gameType  .~ gt
      & otherTeam .~ "foo"

  context "unknown game type" $
    it "should return an empty string" $
      homeTeam (gs Nothing) `shouldBe` ""

  context "home game" $
    it ("should return " ++ show myTeam) $
      homeTeam (gs $ Just HomeGame) `shouldBe` myTeam

  context "away game" $
    it "should return \"foo\"" $
      homeTeam (gs $ Just AwayGame) `shouldBe` "foo"

awayTeamSpec :: Spec
awayTeamSpec = describe "awayTeam" $ do
  let
    gs gt = newGameState
      & gameType  .~ gt
      & otherTeam .~ "foo"

  context "unknown game type" $
    it "should return an empty string" $
      awayTeam (gs Nothing) `shouldBe` ""

  context "home game" $
    it "should return \"foo\"" $
      awayTeam (gs $ Just HomeGame) `shouldBe` "foo"

  context "away game" $
    it ("should return " ++ show myTeam) $
      awayTeam (gs $ Just AwayGame) `shouldBe` myTeam

gameWonSpec :: Spec
gameWonSpec = describe "gameWon" $ mapM_
  (\(t, h, a, expected) -> let
    desc = "game type: " ++ show t ++
      ", home score: " ++ show h ++
      ", away score: " ++ show a
    gs = newGameState
      & gameType  .~ t
      & homeScore .~ h
      & awayScore .~ a
    in context desc $
      it ("should be " ++ show expected) $
        gameWon gs `shouldBe` expected)
  --  gameType,      homeScore, awayScore, expected
  [ ( Just HomeGame, Just 1,    Just 1,    Just False )
  , ( Just HomeGame, Just 1,    Just 2,    Just False )
  , ( Just HomeGame, Just 2,    Just 1,    Just True  )
  , ( Just AwayGame, Just 1,    Just 1,    Just False )
  , ( Just AwayGame, Just 1,    Just 2,    Just True  )
  , ( Just AwayGame, Just 2,    Just 1,    Just False )
  , ( Nothing,       Just 1,    Just 2,    Nothing    )
  , ( Just HomeGame, Nothing,   Just 1,    Nothing    )
  , ( Just AwayGame, Nothing,   Just 1,    Nothing    )
  , ( Just HomeGame, Just 1,    Nothing,   Nothing    )
  , ( Just AwayGame, Just 1,    Nothing,   Nothing    )
  , ( Nothing,       Nothing,   Nothing,   Nothing    )
  ]

gameLostSpec :: Spec
gameLostSpec = describe "gameLost" $ mapM_
  (\(t, h, a, ot, expected) -> let
    desc = "game type: " ++ show t ++
      ", home score: " ++ show h ++
      ", away score: " ++ show a ++
      ", overtimr flag: " ++ show ot
    gs = newGameState
      & gameType     .~ t
      & homeScore    .~ h
      & awayScore    .~ a
      & overtimeFlag .~ ot
    in context desc $
      it ("should be " ++ show expected) $
        gameLost gs `shouldBe` expected)
  --  gameType,      homeScore, awayScore, overtimeFlag, expected
  [ ( Just HomeGame, Just 1,    Just 1,    Just False,   Just False )
  , ( Just HomeGame, Just 1,    Just 2,    Just False,   Just True  )
  , ( Just HomeGame, Just 1,    Just 2,    Just True,    Just False )
  , ( Just HomeGame, Just 2,    Just 1,    Just False,   Just False )
  , ( Just AwayGame, Just 1,    Just 1,    Just False,   Just False )
  , ( Just AwayGame, Just 1,    Just 2,    Just False,   Just False )
  , ( Just AwayGame, Just 2,    Just 1,    Just False,   Just True  )
  , ( Just AwayGame, Just 2,    Just 1,    Just True,    Just False )
  , ( Nothing,       Just 1,    Just 2,    Just False,   Nothing    )
  , ( Just HomeGame, Nothing,   Just 1,    Just False,   Nothing    )
  , ( Just AwayGame, Nothing,   Just 1,    Just False,   Nothing    )
  , ( Just HomeGame, Just 1,    Nothing,   Just False,   Nothing    )
  , ( Just AwayGame, Just 1,    Nothing,   Just False,   Nothing    )
  , ( Just HomeGame, Just 1,    Just 2,    Nothing,      Nothing    )
  , ( Just AwayGame, Just 1,    Just 2,    Nothing,      Nothing    )
  , ( Nothing,       Nothing,   Nothing,   Just False,   Nothing    )
  ]

gameTiedSpec :: Spec
gameTiedSpec = describe "gameTied" $ mapM_
  (\(home, away, expected) -> let
    desc = "home score: " ++ show home ++
      ", away score: " ++ show away
    gs = newGameState
      & homeScore .~ home
      & awayScore .~ away
    in context desc $
      it ("should be " ++ show expected) $
        gameTied gs `shouldBe` expected)
  [ ( Nothing, Nothing, Nothing    )
  , ( Nothing, Just 1,  Nothing    )
  , ( Just 1,  Nothing, Nothing    )
  , ( Just 1,  Just 1,  Just True  )
  , ( Just 1,  Just 2,  Just False )
  ]

unaccountedPointsSpec :: Spec
unaccountedPointsSpec = describe "unaccounted points" $ do
  context "no data" $
    it "should return Nothing" $
      unaccountedPoints newGameState `shouldBe` Nothing

  context "unaccounted points" $
    it "should return True" $ let
      gs = newGameState
        & gameType  ?~ HomeGame
        & homeScore ?~ 1
      in unaccountedPoints gs `shouldBe` Just True

  context "all points accounted" $
    it "should return False" $ let
      gs = newGameState
        & gameType        ?~ HomeGame
        & homeScore       ?~ 1
        & pointsAccounted .~ 1
      in unaccountedPoints gs `shouldBe` Just False

  context "more points accounted" $
    it "should return True" $ let
      gs = newGameState
        & gameType        ?~ HomeGame
        & homeScore       ?~ 1
        & pointsAccounted .~ 2
      in unaccountedPoints gs `shouldBe` Just False

gmsGamesSpec :: Spec
gmsGamesSpec = describe "gmsGames" $ mapM_
  (\(w, l, ot, expected) -> let
    desc = "wins: " ++ show w ++
      ", losses: " ++ show l ++
      ", overtime: " ++ show ot
    gs = newGameStats
      & gmsWins     .~ w
      & gmsLosses   .~ l
      & gmsOvertime .~ ot
    in context desc $
      it ("should be " ++ show expected) $
        gmsGames gs `shouldBe` expected)
  --  wins, losses, overtime, expected
  [ ( 0,    0,      0,        0        )
  , ( 1,    0,      0,        1        )
  , ( 0,    1,      0,        1        )
  , ( 0,    0,      1,        1        )
  , ( 1,    1,      1,        3        )
  , ( 2,    3,      5,        10       )
  ]

gmsPointsSpec :: Spec
gmsPointsSpec = describe "gmsPoints" $ mapM_
  (\(w, l, ot, expected) -> let
    gs
      = newGameStats
      & gmsWins     .~ w
      & gmsLosses   .~ l
      & gmsOvertime .~ ot
    in context (show gs) $
      it ("should be " ++ show expected) $
        gmsPoints gs `shouldBe` expected)
  --  wins, losses, overtime, expected
  [ ( 0,    0,      0,        0        )
  , ( 1,    0,      0,        2        )
  , ( 0,    1,      0,        0        )
  , ( 0,    1,      1,        1        )
  , ( 1,    1,      1,        3        )
  , ( 2,    4,      3,        7        )
  ]

addGameStatsSpec :: Spec
addGameStatsSpec = describe "addGameStats" $
  it "should add the values" $ let

    s1 = GameStats
      { _gmsWins         = 1
      , _gmsLosses       = 2
      , _gmsOvertime     = 3
      , _gmsGoalsFor     = 4
      , _gmsGoalsAgainst = 5
      }

    s2 = GameStats
      { _gmsWins         = 6
      , _gmsLosses       = 7
      , _gmsOvertime     = 8
      , _gmsGoalsFor     = 9
      , _gmsGoalsAgainst = 10
      }

    expected = GameStats
      { _gmsWins         = 7
      , _gmsLosses       = 9
      , _gmsOvertime     = 11
      , _gmsGoalsFor     = 13
      , _gmsGoalsAgainst = 15
      }

    in addGameStats s1 s2 `shouldBe` expected

playerSearchSpec :: Spec
playerSearchSpec = describe "playerSearch" $ mapM_
  (\(sStr, expected) -> context sStr $
    it ("should return " ++ show expected) $ let
      ps = [joe, bob, steve]
      in playerSearch sStr ps `shouldBe` expected)
  --  search, result
  [ ( "joe",  [(0, joe)]             )
  , ( "o",    [(0, joe), (1, bob)]   )
  , ( "e",    [(0, joe), (2, steve)] )
  , ( "x",    []                     )
  ]

playerSearchExactSpec :: Spec
playerSearchExactSpec = describe "playerSearchExact" $ mapM_
  (\(sStr, expected) -> context sStr $
    it ("should be " ++ show expected) $ let
      ps = [joe, bob, steve]
      in playerSearchExact sStr ps `shouldBe` expected)
  --  search,  result
  [ ( "Joe",   Just (0, joe)   )
  , ( "Bob",   Just (1, bob)   )
  , ( "Steve", Just (2, steve) )
  , ( "Sam",   Nothing         )
  , ( "",      Nothing         )
  ]

modifyPlayerSpec :: Spec
modifyPlayerSpec = describe "modifyPlayer" $ mapM_
  (\(name, j, b, s) -> let
    modifier = pLifetime.psGoals .~ 1
    players = modifyPlayer modifier name [joe, bob, steve]
    in context ("modify " ++ name) $ do

      context "Joe's lifetime goals" $
        it ("should be " ++ show j) $
          head players^.pLifetime.psGoals `shouldBe` j

      context "Bob's lifetime goals" $
        it ("should be " ++ show b) $
          (players !! 1)^.pLifetime.psGoals `shouldBe` b

      context "Steve's lifetime goals" $
        it ("should be " ++ show s) $
          last players^.pLifetime.psGoals `shouldBe` s)
  --  player name, Joe's goals, Bob's goals, Steve's goals
  [ ( "Joe",       1,           0,           0             )
  , ( "Bob",       0,           1,           0             )
  , ( "Steve",     0,           0,           1             )
  , ( "Sam",       0,           0,           0             )
  ]

playerSummarySpec :: Spec
playerSummarySpec = describe "playerSummary" $
  it "should be \"Joe (2) center\"" $
    playerSummary joe `shouldBe` "Joe (2) center"

playerIsActiveSpec :: Spec
playerIsActiveSpec = describe "playerIsActive" $ do
  let
    pStats = newPlayerStats
      & psGoals   .~ 10
      & psAssists .~ 11
      & psPMin    .~ 12
    p = newPlayer 1 "Joe" "centre" & pLifetime .~ pStats

  mapM_
    (\(label, p', expected) -> context label $
      it ("should be " ++ show expected) $
        playerIsActive p' `shouldBe` expected)
    --  label,                player,                  expected
    [ ( "not active",         p,                       False    )
    , ( "has goal",           p & pYtd.psGoals   .~ 1, True     )
    , ( "has assist",         p & pYtd.psAssists .~ 1, True     )
    , ( "has penalty minute", p & pYtd.psPMin    .~ 1, True     )
    ]

psPointsSpec :: Spec
psPointsSpec = describe "psPoints" $ mapM_
  (\(goals, assists, points) -> let
    desc = "goals: " ++ show goals ++
      ", assists: " ++ show assists
    stats = newPlayerStats &
      psGoals   .~ goals &
      psAssists .~ assists
    in context desc $
      it ("should be " ++ show points) $
        psPoints stats `shouldBe` points)
  --  goals, assists, points
  [ ( 0,     0,       0      )
  , ( 1,     0,       1      )
  , ( 0,     1,       1      )
  , ( 2,     3,       5      )
  ]

addPlayerStatsSpec :: Spec
addPlayerStatsSpec = describe "addPlayerStats" $ do
  let
    s1
      = newPlayerStats
      & psGoals   .~ 1
      & psAssists .~ 2
      & psPMin    .~ 3
    s2
      = newPlayerStats
      & psGoals   .~ 4
      & psAssists .~ 5
      & psPMin    .~ 6
    s3 = addPlayerStats s1 s2

  describe "psGoals" $
    it "should be 5" $
      s3^.psGoals `shouldBe` 5

  describe "psAssists" $
    it "should be 7" $
      s3^.psAssists `shouldBe` 7

  describe "psPMin" $
    it "should be 9" $
      s3^.psPMin `shouldBe` 9

goalieSearchSpec :: Spec
goalieSearchSpec = describe "goalieSearch" $ do
  let
    goalies =
      [ newGoalie 2 "Joe"
      , newGoalie 3 "Bob"
      , newGoalie 5 "Steve"
      ]
    result n = (n, goalies!!n)

  context "partial match" $
    it "should return Joe and Steve" $
      goalieSearch "e" goalies `shouldBe` [result 0, result 2]

  context "no match" $
    it "should return an empty list" $
      goalieSearch "x" goalies `shouldBe` []

  context "exact match" $
    it "should return Bob" $
      goalieSearch "bob" goalies `shouldBe` [result 1]

goalieSearchExactSpec :: Spec
goalieSearchExactSpec = describe "goalieSearchExact" $ do
  let
    goalies =
      [ newGoalie 2 "Joe"
      , newGoalie 3 "Bob"
      , newGoalie 5 "Steve"
      ]
    result n = (n, goalies!!n)

  mapM_
    (\(name, num) -> context name $
      it ("should return " ++ name) $
        goalieSearchExact name goalies `shouldBe` Just (result num))
    --  name,    num
    [ ( "Joe",   0   )
    , ( "Bob",   1   )
    , ( "Steve", 2   )
    ]

  context "Greg" $
    it "should return Nothing" $
      goalieSearchExact "Greg" goalies `shouldBe` Nothing

goalieSummarySpec :: Spec
goalieSummarySpec = describe "goalieSummary" $
  it "should provide a summary string" $
    goalieSummary (newGoalie 2 "Joe") `shouldBe` "Joe (2)"

goalieIsActiveSpec :: Spec
goalieIsActiveSpec = describe "goalieIsActive" $ mapM_
  (\(label, input, expected) -> context label $
    it ("should be " ++ show expected) $
      goalieIsActive input `shouldBe` expected)

  --  label,      input,    expected
  [ ( "inactive", inactive, False    )
  , ( "active",   active,   True     )
  ]

  where
    inactive = newGoalie 1 "Joe"
      & gLifetime.gsMinsPlayed .~ 1

    active = inactive
      & gYtd.gsMinsPlayed .~ 1

addGoalieStatsSpec :: Spec
addGoalieStatsSpec = describe "addGoalieStats" $ let
  g1 = GoalieStats
    { _gsGames        = 1
    , _gsMinsPlayed   = 2
    , _gsGoalsAllowed = 3
    , _gsShutouts     = 4
    , _gsWins         = 5
    , _gsLosses       = 6
    , _gsTies         = 7
    }

  g2 = GoalieStats
    { _gsGames        = 8
    , _gsMinsPlayed   = 9
    , _gsGoalsAllowed = 10
    , _gsShutouts     = 11
    , _gsWins         = 12
    , _gsLosses       = 13
    , _gsTies         = 14
    }

  expected = GoalieStats
    { _gsGames        = 9
    , _gsMinsPlayed   = 11
    , _gsGoalsAllowed = 13
    , _gsShutouts     = 15
    , _gsWins         = 17
    , _gsLosses       = 19
    , _gsTies         = 21
    }

  actual = g1 `addGoalieStats` g2

  in it ("should be " ++ show expected) $
    actual `shouldBe` expected

gsAverageSpec :: Spec
gsAverageSpec = describe "gsAverage" $ mapM_
  (\(label, stats, expected) -> context label $
    it ("should be " ++ show expected) $
      gsAverage stats `shouldBe` expected)

  --  label,          stats,           expected
  [ ( "with minutes", gs,              3 % 2    )
  , ( "no minutes",   newGoalieStats , 0        )
  ]

  where
    gs = newGoalieStats
      & gsMinsPlayed   .~ 2 * gameLength
      & gsGoalsAllowed .~ 3

joe :: Player
joe = newPlayer 2 "Joe" "center"

bob :: Player
bob = newPlayer 3 "Bob" "defense"

steve :: Player
steve = newPlayer 5 "Steve" "forward"

-- | Creates a 'Player'
makePlayer :: IO Player
makePlayer = Player
  <$> makeNum
  <*> makeName
  <*> makeName
  <*> makeBool
  <*> makeBool
  <*> makePlayerStats
  <*> makePlayerStats

-- | Creates a 'Goalie'
makeGoalie :: IO Goalie
makeGoalie = Goalie
  <$> makeNum
  <*> makeName
  <*> makeBool
  <*> makeBool
  <*> makeGoalieStats
  <*> makeGoalieStats

-- | Creates a 'PlayerStats' value
makePlayerStats :: IO PlayerStats
makePlayerStats = PlayerStats
  <$> makeNum
  <*> makeNum
  <*> makeNum

-- | Creates a 'GoalieStats' value
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

makeBool :: IO Bool
makeBool = randomIO

makeName :: IO String
makeName = replicateM 10 $ randomRIO ('A', 'Z')

instance Comparable GoalieStats where
  compareTest actual expected = mapM_
    (\(name, lens) -> describe name $
      it ("should be " ++ show (expected^.lens)) $
        actual^.lens `shouldBe` expected^.lens)
    --  name,             lens
    [ ( "gsGames",        gsGames        )
    , ( "gsMinsPlayed",   gsMinsPlayed   )
    , ( "gsGoalsAllowed", gsGoalsAllowed )
    , ( "gsWins",         gsWins         )
    , ( "gsLosses",       gsLosses       )
    , ( "gsTies",         gsTies         )
    ]

instance Comparable GameState where
  compareTest actual expected =
    it ("should be " ++ show expected) $
      actual `shouldBe` expected

instance Comparable CreatePlayerState where
  compareTest actual expected = do

    describe "cpsNumber" $
      it ("should be " ++ show (expected^.cpsNumber)) $
        actual^.cpsNumber `shouldBe` expected^.cpsNumber

    describe "cpsName" $
      it ("should be " ++ expected^.cpsName) $
        actual^.cpsName `shouldBe` expected^.cpsName

    describe "cpsPosition" $
      it ("should be " ++ expected^.cpsPosition) $
        actual^.cpsPosition `shouldBe` expected^.cpsPosition

instance Comparable EditPlayerState where
  compareTest actual expected = do

    describe "epsSelectedPlayer" $
      it ("should be " ++ show (expected^.epsSelectedPlayer)) $
        actual^.epsSelectedPlayer `shouldBe` expected^.epsSelectedPlayer

    describe "epsMode" $
      it ("should be " ++ show (expected^.epsMode)) $
        actual^.epsMode `shouldBe` expected^.epsMode

instance Comparable EditGoalieState where
  compareTest actual expected = do

    describe "egsSelectedGoalie" $
      it ("should be " ++ show (expected^.egsSelectedGoalie)) $
        actual^.egsSelectedGoalie `shouldBe` expected^.egsSelectedGoalie

    describe "egsMode" $
      it ("should be " ++ show (expected^.egsMode)) $
        actual^.egsMode `shouldBe` expected^.egsMode

instance Comparable CreateGoalieState where
  compareTest actual expected = do

    describe "cgsNuber" $
      it("should be " ++ show (expected^.cgsNumber)) $
        actual^.cgsNumber `shouldBe` expected^.cgsNumber

    describe "cgsName" $
      it ("should be " ++ expected^.cgsName) $
        actual^.cgsName `shouldBe` expected^.cgsName
