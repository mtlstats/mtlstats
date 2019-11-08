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

module Actions.NewGame.GoalieInputSpec (spec) where

import qualified Data.Map as M
import Data.Maybe (fromJust)
import Lens.Micro ((^.), (&), (.~), (?~))
import Test.Hspec (Spec, context, describe, it, shouldBe)

import Mtlstats.Actions.NewGame.GoalieInput
import Mtlstats.Types
import Mtlstats.Util

import qualified TypesSpec as TS

spec :: Spec
spec = describe "Mtlstats.Actions.GoalieInput" $ do
  finishGoalieEntrySpec
  recordGoalieStatsSpec
  setGameGoalieSpec

finishGoalieEntrySpec :: Spec
finishGoalieEntrySpec = describe "finishGoalieEntry" $ do
  let
    progState stats = newProgState
      & progMode.gameStateL.gameGoalieStats .~ stats
      & finishGoalieEntry

  context "no goalie data" $
    it "should not set goaliesRecorded" $ let
      s = progState M.empty
      in s^.progMode.gameStateL.gameGoaliesRecorded `shouldBe` False

  context "goalie data" $
    it "should set goaliesRecorded" $ let
      s = progState $ M.fromList [(1, newGoalieStats)]
      in s^.progMode.gameStateL.gameGoaliesRecorded `shouldBe` True

recordGoalieStatsSpec :: Spec
recordGoalieStatsSpec = describe "recordGoalieStats" $ let
  goalieStats games mins goals = newGoalieStats
    & gsGames        .~ games
    & gsMinsPlayed   .~ mins
    & gsGoalsAllowed .~ goals

  joe = newGoalie 2 "Joe"
    & gYtd      .~ goalieStats 10 11 12
    & gLifetime .~ goalieStats 20 21 22

  bob = newGoalie 3 "Bob"
    & gYtd      .~ goalieStats 30 31 32
    & gLifetime .~ goalieStats 40 41 42

  gameState n mins goals = newGameState
    & gameGoalieStats      .~ M.fromList [(1, goalieStats 1 2 3)]
    & gameSelectedGoalie   .~ n
    & gameGoalieMinsPlayed .~ mins
    & gameGoalsAllowed     .~ goals

  progState n mins goals = newProgState
    & database.dbGoalies  .~ [joe, bob]
    & progMode.gameStateL .~ gameState n mins goals

  in mapM_
    (\(name, gid, mins, goals, joeData, bobData, reset) -> let
      s = recordGoalieStats $ progState gid mins goals
      in context name $ do

        mapM_
          (\( name
            , gid
            , ( gGames
              , gMins
              , gGoals
              , ytdGames
              , ytdMins
              , ytdGoals
              , ltGames
              , ltMins
              , ltGoals
              )
            ) -> context name $ do
              let
                gs     = s^.progMode.gameStateL.gameGoalieStats
                game   = M.findWithDefault newGoalieStats gid gs
                goalie = fromJust $ nth gid $ s^.database.dbGoalies
                ytd    = goalie^.gYtd
                lt     = goalie^.gLifetime

              context "game" $
                game `TS.compareTest` goalieStats gGames gMins gGoals

              context "year-to-date" $
                ytd `TS.compareTest` goalieStats ytdGames ytdMins ytdGoals

              context "lifetime" $
                lt `TS.compareTest` goalieStats ltGames ltMins ltGoals)

          [ ( "checking Joe", 0, joeData )
          , ( "checking Bob", 1, bobData )
          ]

        context "selected goalie" $ let
          expected = if reset then Nothing else gid
          in it ("should be " ++ show expected) $
            (s^.progMode.gameStateL.gameSelectedGoalie) `shouldBe` expected

        context "minutes played" $ let
          expected = if reset then Nothing else mins
          in it ("should be " ++ show expected) $
            (s^.progMode.gameStateL.gameGoalieMinsPlayed) `shouldBe` expected

        context "goals allowed" $ let
          expected = if reset then Nothing else goals
          in it ("should be " ++ show expected) $
            (s^.progMode.gameStateL.gameGoalsAllowed) `shouldBe` expected)

    [ ( "updating Joe"
      , Just 0
      , Just 1
      , Just 2
      , (1, 1, 2, 11, 12, 14, 21, 22, 24)
      , (1, 2, 3, 30, 31, 32, 40, 41, 42)
      , True
      )
    , ( "updating Bob"
      , Just 1
      , Just 1
      , Just 2
      , (0, 0, 0, 10, 11, 12, 20, 21, 22)
      , (1, 3, 5, 30, 32, 34, 40, 42, 44)
      , True
      )
    , ( "goalie out of bounds"
      , Just 2
      , Just 1
      , Just 2
      , (0, 0, 0, 10, 11, 12, 20, 21, 22)
      , (1, 2, 3, 30, 31, 32, 40, 41, 42)
      , False
      )
    , ( "missing goalie"
      , Nothing
      , Just 1
      , Just 2
      , (0, 0, 0, 10, 11, 12, 20, 21, 22)
      , (1, 2, 3, 30, 31, 32, 40, 41, 42)
      , False
      )
    , ( "missing minutes"
      , Just 0
      , Nothing
      , Just 1
      , (0, 0, 0, 10, 11, 12, 20, 21, 22)
      , (1, 2, 3, 30, 31, 32, 40, 41, 42)
      , False
      )
    , ( "missing goals"
      , Just 0
      , Just 1
      , Nothing
      , (0, 0, 0, 10, 11, 12, 20, 21, 22)
      , (1, 2, 3, 30, 31, 32, 40, 41, 42)
      , False
      )
    ]

setGameGoalieSpec :: Spec
setGameGoalieSpec = describe "setGameGoalie" $ let

  goalieStats w l t = newGoalieStats
    & gsWins   .~ w
    & gsLosses .~ l
    & gsTies   .~ t

  bob = newGoalie 2 "Bob"
    & gYtd      .~ goalieStats 10 11 12
    & gLifetime .~ goalieStats 20 21 22

  joe = newGoalie 3 "Joe"
    & gYtd      .~ goalieStats 30 31 32
    & gLifetime .~ goalieStats 40 41 42

  gameState h a ot = newGameState
    & gameType        ?~ HomeGame
    & homeScore       ?~ h
    & awayScore       ?~ a
    & overtimeFlag    ?~ ot

  winningGame = gameState 1 0 False
  losingGame  = gameState 0 1 False
  tiedGame    = gameState 0 1 True

  in mapM_
    (\(label, gameState, gid, bobData, joeData) -> context label $ let

      progState = newProgState
        & database.dbGoalies  .~ [bob, joe]
        & progMode.gameStateL .~ gameState
        & setGameGoalie gid

      in mapM_
        (\( label
          , gid
          , ( gWins
            , gLosses
            , gTies
            , ytdWins
            , ytdLosses
            , ytdTies
            , ltWins
            , ltLosses
            , ltTies
            )
          ) -> context label $ do
            let
              goalie = (progState^.database.dbGoalies) !! gid
              gameStats = progState^.progMode.gameStateL.gameGoalieStats
              game      = M.findWithDefault newGoalieStats gid gameStats
              ytd       = goalie^.gYtd
              lifetime  = goalie^.gLifetime

            mapM_
              (\(label, expected, actual) -> context label $
                expected `TS.compareTest` actual)
              [ ( "game stats",     game,     goalieStats gWins   gLosses   gTies   )
              , ( "YTD stats",      ytd,      goalieStats ytdWins ytdLosses ytdTies )
              , ( "lifetime stats", lifetime, goalieStats ltWins  ltLosses  ltTies  )
              ]

            it "should set the gameGoalieAssigned flag" $
              progState^.progMode.gameStateL.gameGoalieAssigned `shouldBe` True)
        [ ( "checking Bob", 0, bobData )
        , ( "checking Joe", 1, joeData )
        ])
    [ ( "Bob wins"
      , winningGame
      , 0
      , ( 1, 0, 0, 11, 11, 12, 21, 21, 22 )
      , ( 0, 0, 0, 30, 31, 32, 40, 41, 42 )
      )
    , ( "Bob loses"
      , losingGame
      , 0
      , ( 0, 1, 0, 10, 12, 12, 20, 22, 22 )
      , ( 0, 0, 0, 30, 31, 32, 40, 41, 42 )
      )
    , ( "Bob ties"
      , tiedGame
      , 0
      , ( 0, 0, 1, 10, 11, 13, 20, 21, 23 )
      , ( 0, 0, 0, 30, 31, 32, 40, 41, 42 )
      )
    , ( "Joe wins"
      , winningGame
      , 1
      , ( 0, 0, 0, 10, 11, 12, 20, 21, 22 )
      , ( 1, 0, 0, 31, 31, 32, 41, 41, 42 )
      )
    , ( "Joe loses"
      , losingGame
      , 1
      , ( 0, 0, 0, 10, 11, 12, 20, 21, 22 )
      , ( 0, 1, 0, 30, 32, 32, 40, 42, 42 )
      )
    , ( "Joe ties"
      , tiedGame
      , 1
      , ( 0, 0, 0, 10, 11, 12, 20, 21, 22 )
      , ( 0, 0, 1, 30, 31, 33, 40, 41, 43 )
      )
    ]
