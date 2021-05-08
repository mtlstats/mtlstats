{-

mtlstats
Copyright (C) 1984, 1985, 2019, 2020, 2021 Rhéal Lamothe
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
import Lens.Micro ((^.), (&), (.~), (?~), (%~))
import Test.Hspec (Spec, context, describe, it, shouldBe)

import Mtlstats.Actions.NewGame.GoalieInput
import Mtlstats.Types
import Mtlstats.Util

import qualified TypesSpec as TS

spec :: Spec
spec = describe "GoalieInput" $ do
  finishGoalieEntrySpec
  recordGoalieStatsSpec
  setGameGoalieSpec

finishGoalieEntrySpec :: Spec
finishGoalieEntrySpec = describe "finishGoalieEntry" $ mapM_
  (\(label, stats, grFlag, gaFlag) -> context label $ do
    let
      ps = newProgState
        & progMode.gameStateL
          %~ (gameGoalieStats .~ stats)
          .  (gameType        ?~ HomeGame)
          .  (homeScore       ?~ 1)
          .  (awayScore       ?~ 0)
          .  (overtimeFlag    ?~ False)
        & database.dbGoalies .~ goalies

      ps' = finishGoalieEntry ps
      gs  = ps'^.progMode.gameStateL

    describe "gameGoaliesRecorded" $
      it ("should be " ++ show grFlag) $
        gs^.gameGoaliesRecorded `shouldBe` grFlag

    describe "gameGoalieAssigned" $
      it ("should be " ++ show gaFlag) $
        gs^.gameGoalieAssigned `shouldBe` gaFlag)

  --  label,         initial stats, goalies recorded, goalie assigned
  [ ( "no goalies",  noGoalies,     False,            False           )
  , ( "one goalie",  oneGoalie,     True,             True            )
  , ( "two goalies", twoGoalies,    True,             False           )
  ]

  where
    goalies    = [joe, bob]
    joe        = newGoalie 2 "Joe"
    bob        = newGoalie 3 "Bob"
    noGoalies  = M.empty
    oneGoalie  = M.fromList [joeStats]
    twoGoalies = M.fromList [joeStats, bobStats]
    joeStats   = (0, newGoalieStats)
    bobStats   = (1, newGoalieStats)

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
    (\(setName, setGid, mins, goals, joeData, bobData, reset) -> let
      s = recordGoalieStats $ progState setGid mins goals
      in context setName $ do

        mapM_
          (\( chkName
            , chkGid
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
            ) -> context chkName $ do
              let
                gs     = s^.progMode.gameStateL.gameGoalieStats
                game   = M.findWithDefault newGoalieStats chkGid gs
                goalie = fromJust $ nth chkGid $ s^.database.dbGoalies
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
          expected = if reset then Nothing else setGid
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
setGameGoalieSpec = describe "setGameGoalie" $ mapM_

  (\(label, goalieId, ps, expectedJoe, expectedBob, expectedGStats) ->
    context label $ do

      let
        ps'          = setGameGoalie goalieId ps
        [joe', bob'] = ps'^.database.dbGoalies
        gStats'      = ps'^.progMode.gameStateL.gameGoalieStats

      context "Joe" $ joe' `TS.compareTest` expectedJoe
      context "Bob" $ bob' `TS.compareTest` expectedBob
      context "game stats" $ gStats' `TS.compareTest` expectedGStats)

  [ ( "Joe wins - no shutout"
    , 0
    , psWin
    , joeWin
    , bob
    , gsJoeWin
    )

  , ( "Bob wins - no shutout"
    , 1
    , psWin
    , joe
    , bobWin
    , gsBobWin
    )

  , ( "Joe wins - shutout"
    , 0
    , psWinSO
    , joeWinSO
    , bob
    , gsJoeWinSO
    )

  , ( "Bob wins - shutout"
    , 1
    , psWinSO
    , joe
    , bobWinSO
    , gsBobWinSO
    )

  , ( "Joe loses"
    , 0
    , psLose
    , joeLose
    , bob
    , gsJoeLose
    )

  , ( "Bob loses"
    , 1
    , psLose
    , joe
    , bobLose
    , gsBobLose
    )

  , ( "Joe overtime"
    , 0
    , psOT
    , joeOT
    , bob
    , gsJoeOT
    )

  , ( "Bob overtime"
    , 1
    , psOT
    , joe
    , bobOT
    , gsBobOT
    )
  ]

  where

    joe
      = newGoalie 2 "Joe"
      & gYtd
        %~ (gsShutouts .~ 11)
        .  (gsWins     .~ 12)
        .  (gsLosses   .~ 13)
        .  (gsTies     .~ 14)
      & gLifetime
        %~ (gsShutouts .~ 21)
        .  (gsWins     .~ 22)
        .  (gsLosses   .~ 23)
        .  (gsTies     .~ 24)

    bob
      = newGoalie 3 "Bob"
      & gYtd
        %~ (gsShutouts .~ 31)
        .  (gsWins     .~ 32)
        .  (gsLosses   .~ 33)
        .  (gsTies     .~ 34)
      & gLifetime
        %~ (gsShutouts .~ 41)
        .  (gsWins     .~ 42)
        .  (gsLosses   .~ 43)
        .  (gsTies     .~ 44)

    joeWin   = win joe
    bobWin   = win bob
    joeWinSO = winSO joe
    bobWinSO = winSO bob
    joeLose  = lose joe
    bobLose  = lose bob
    joeOT    = tie joe
    bobOT    = tie bob

    psWin = mkProgState
      $ (homeScore ?~ 2)
      . (awayScore ?~ 1)

    psWinSO = mkProgState
      $ (homeScore ?~ 1)
      . (awayScore ?~ 0)

    psLose = mkProgState
      $ (homeScore ?~ 0)
      . (awayScore ?~ 1)

    psOT = mkProgState
      $ (homeScore    ?~ 0)
      . (awayScore    ?~ 1)
      . (overtimeFlag ?~ True)

    mkProgState f
      = newProgState
      & database.dbGoalies  .~ [joe, bob]
      & progMode.gameStateL
        %~ f
        .  (gameType     ?~ HomeGame)
        .  (overtimeFlag ?~ False)

    gsJoeWin   = mkGameStats 0 incWin
    gsBobWin   = mkGameStats 1 incWin
    gsJoeWinSO = mkGameStats 0 $ incWin . incSO
    gsBobWinSO = mkGameStats 1 $ incWin . incSO
    gsJoeLose  = mkGameStats 0 incLoss
    gsBobLose  = mkGameStats 1 incLoss
    gsJoeOT    = mkGameStats 0 incOT
    gsBobOT    = mkGameStats 1 incOT

    mkGameStats n f = M.fromList [(n, f newGoalieStats)]

    win
      = (gYtd      %~ incWin)
      . (gLifetime %~ incWin)

    winSO
      = (gYtd      %~ (incWin . incSO))
      . (gLifetime %~ (incWin . incSO))

    lose
      = (gYtd      %~ incLoss)
      . (gLifetime %~ incLoss)

    tie
      = (gYtd      %~ incOT)
      . (gLifetime %~ incOT)

    incWin  = gsWins     %~ succ
    incSO   = gsShutouts %~ succ
    incLoss = gsLosses   %~ succ
    incOT   = gsTies     %~ succ
