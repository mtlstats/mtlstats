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
import Lens.Micro ((^.), (&), (.~), (?~), (%~))
import Test.Hspec (Spec, context, describe, it, shouldBe)

import Mtlstats.Actions.NewGame.GoalieInput
import Mtlstats.Config
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
recordGoalieStatsSpec = describe "recordGoalieStats" $ mapM_
  ( \(label, input, expected) ->
    context label $ do
      let ps = recordGoalieStats input
      ps `TS.compareTest` expected
  )

  [ ( "No goalie"
    , noGoalie
    , noGoalie
    )

  , ( "Missing goalie"
    , missingGoalie
    , missingGoalie
    )

  , ( "Partial game - single goalie"
    , partialSingle
    , partialSingle'
    )

  , ( "Partial game - dual goalie"
    , partialDual
    , partialDual'
    )

  , ( "Full game - no shut out"
    , fullNoSO
    , fullNoSO'
    )

  , ( "Full game - shutout"
    , fullSO
    , fullSO'
    )
  ]

  where
    noGoalie = defProgState 2 1

    missingGoalie = defProgState 2 1
      & setGoalie 99 gameLength 0

    partialSingle = defProgState 2 1
      & setGoalie 0 partialGame 0

    partialSingle' = partialSingle
      & clearGoalie
      . addStats 0 (mkStats partialGame 0)

    partialDual = defProgState 2 1
      & setGoalie 1 partialGame 0
      . addStats 0 (mkStats partialGame 1)

    partialDual' = partialDual
      & clearGoalie
      . addStats 1 (mkStats partialGame 0)

    fullNoSO = defProgState 2 1
      & setGoalie 0 gameLength 1

    fullNoSO' = fullNoSO
      & (progMode .~ MainMenu)
      . (updateStats 0 $ gsWins %~ succ)

    fullSO = defProgState 1 0
      & setGoalie 0 gameLength 0

    fullSO' = fullSO
      & (progMode .~ MainMenu)
      . ( updateStats 0
          $ ( gsWins     %~ succ )
          . ( gsShutouts %~ succ )
        )

    setGoalie g mp ga = progMode.gameStateL
      %~ ( gameSelectedGoalie   ?~ g  )
      .  ( gameGoalieMinsPlayed ?~ mp )
      .  ( gameGoalsAllowed     ?~ ga )

    clearGoalie = progMode.gameStateL
      %~ ( gameSelectedGoalie   .~ Nothing )
      .  ( gameGoalieMinsPlayed .~ Nothing )
      .  ( gameGoalsAllowed     .~ Nothing )

    addStats g s = progMode.gameStateL.gameGoalieStats
      %~ M.insert g s

    updateStats g f = database.dbGoalies
      %~ modifyNth g
         ( ( gYtd      %~ f )
         . ( gLifetime %~ f )
         )

    mkStats mp ga = newGoalieStats
      & ( gsMinsPlayed   .~ mp )
      . ( gsGoalsAllowed .~ ga )

    defProgState h a = newProgState
      & progMode.gameStateL
        %~ ( gameType  ?~ HomeGame )
        .  ( homeScore ?~ h        )
        .  ( awayScore ?~ a        )
      & database.dbGoalies .~ [jim, bob, steve]

    jim   = mkGoalie 2 "Jim"   1
    bob   = mkGoalie 3 "Bob"   2
    steve = mkGoalie 5 "Steve" 3

    mkGoalie num name n = newGoalie num name
      & gYtd
        %~ ( gsGames        .~ n     )
        .  ( gsMinsPlayed   .~ n + 1 )
        .  ( gsGoalsAllowed .~ n + 2 )
        .  ( gsShutouts     .~ n + 3 )
        .  ( gsWins         .~ n + 4 )
        .  ( gsLosses       .~ n + 5 )
        .  ( gsTies         .~ n + 6 )
      & gLifetime
        %~ ( gsGames        .~ n + 7  )
        .  ( gsMinsPlayed   .~ n + 8  )
        .  ( gsGoalsAllowed .~ n + 9  )
        .  ( gsShutouts     .~ n + 10 )
        .  ( gsWins         .~ n + 11 )
        .  ( gsLosses       .~ n + 12 )
        .  ( gsTies         .~ n + 13 )

    partialGame = pred gameLength

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
