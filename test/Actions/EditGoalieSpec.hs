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

module Actions.EditGoalieSpec (spec) where

import Data.Maybe (fromJust)
import Lens.Micro ((^.), (&), (.~))
import Test.Hspec (Spec, context, describe, it, shouldBe)

import Mtlstats.Actions.EditGoalie
import Mtlstats.Types
import Mtlstats.Util

spec :: Spec
spec = describe "EditGoalie" $ do
  editGoalieNumberSpec
  editGoalieNameSpec
  editGoalieYtdGamesSpec
  editGoalieYtdMinsSpec
  editGoalieYtdGoalsSpec
  editGoalieYtdWinsSpec
  editGoalieYtdLossesSpec

editGoalieNumberSpec :: Spec
editGoalieNumberSpec = describe "editGoalieNumber" $ editTest
  (editGoalieNumber 5)
  EGNumber
  (uncurry newGoalie)
  [ ( "set Joe"
    , Just 0
    , (5, "Joe")
    , (3, "Bob")
    , EGMenu
    )
  , ( "set Bob"
    , Just 1
    , (2, "Joe")
    , (5, "Bob")
    , EGMenu
    )
  , ( "out of bounds"
    , Just 2
    , (2, "Joe")
    , (3, "Bob")
    , EGNumber
    )
  , ( "no goalie selected"
    , Nothing
    , (2, "Joe")
    , (3, "Bob")
    , EGNumber
    )
  ]

editGoalieNameSpec :: Spec
editGoalieNameSpec = describe "editGoalieName" $ editTest
  (editGoalieName "foo")
  EGName
  (uncurry newGoalie)
  [ ( "set Joe"
    , Just 0
    , ( 2, "foo" )
    , ( 3, "Bob" )
    , EGMenu
    )
  , ( "set Bob"
    , Just 1
    , ( 2, "Joe" )
    , ( 3, "foo" )
    , EGMenu
    )
  , ( "out of bounds"
    , Just 2
    , ( 2, "Joe" )
    , ( 3, "Bob" )
    , EGName
    )
  , ( "no goalie selected"
    , Nothing
    , ( 2, "Joe" )
    , ( 3, "Bob" )
    , EGName
    )
  ]

editGoalieYtdGamesSpec :: Spec
editGoalieYtdGamesSpec = describe "editGoalieYtdGames" $ editTest
  (editGoalieYtdGames 1)
  EGYtdGames
  (\(num, name, games) -> newGoalie num name & gYtd.gsGames .~ games)
  [ ( "set Joe"
    , Just 0
    , ( 2, "Joe", 1 )
    , ( 3, "Bob", 0 )
    , EGYtd
    )
  , ( "set Bob"
    , Just 1
    , ( 2, "Joe", 0 )
    , ( 3, "Bob", 1 )
    , EGYtd
    )
  , ( "out of bounds"
    , Just 2
    , ( 2, "Joe", 0 )
    , ( 3, "Bob", 0 )
    , EGYtdGames
    )
  , ( "no goalie selected"
    , Nothing
    , ( 2, "Joe", 0 )
    , ( 3, "Bob", 0 )
    , EGYtdGames
    )
  ]

editGoalieYtdMinsSpec :: Spec
editGoalieYtdMinsSpec = describe "editGoalieYtdMins" $ editTest
  (editGoalieYtdMins 1)
  EGYtdMins
  (\(num, name, mins) -> newGoalie num name & gYtd.gsMinsPlayed .~ mins)
  [ ( "set Joe"
    , Just 0
    , ( 2, "Joe", 1 )
    , ( 3, "Bob", 0 )
    , EGYtd
    )
  , ( "set Bob"
    , Just 1
    , (2, "Joe", 0 )
    , (3, "Bob", 1 )
    , EGYtd
    )
  , ( "out of bounds"
    , Just 2
    , ( 2, "Joe", 0 )
    , ( 3, "Bob", 0 )
    , EGYtdMins
    )
  , ( "no goalie selected"
    , Nothing
    , ( 2, "Joe", 0 )
    , ( 3, "Bob", 0 )
    , EGYtdMins
    )
  ]

editGoalieYtdGoalsSpec :: Spec
editGoalieYtdGoalsSpec = describe "editGoalieYtdGoals" $ editTest
  (editGoalieYtdGoals 1)
  EGYtdGoals
  (\(num, name, goals) -> newGoalie num name & gYtd.gsGoalsAllowed .~ goals)
  [ ( "set Joe"
    , Just 0
    , ( 2, "Joe", 1 )
    , ( 3, "Bob", 0 )
    , EGYtd
    )
  , ( "set Bob"
    , Just 1
    , ( 2, "Joe", 0 )
    , ( 3, "Bob", 1 )
    , EGYtd
    )
  , ( "out of bounds"
    , Just 2
    , ( 2, "Joe", 0 )
    , ( 3, "Bob", 0 )
    , EGYtdGoals
    )
  , ( "no goalie selected"
    , Nothing
    , ( 2, "Joe", 0 )
    , ( 3, "Bob", 0 )
    , EGYtdGoals
    )
  ]

editGoalieYtdWinsSpec :: Spec
editGoalieYtdWinsSpec = describe "editGoalieYtdWins" $ editTest
  (editGoalieYtdWins 1)
  EGYtdWins
  (\(num, name, wins) -> newGoalie num name & gYtd.gsWins .~ wins)
  [ ( "set Joe"
    , Just 0
    , ( 2, "Joe", 1 )
    , ( 3, "Bob", 0 )
    , EGYtd
    )
  , ( "set Bob"
    , Just 1
    , ( 2, "Joe", 0 )
    , ( 3, "Bob", 1 )
    , EGYtd
    )
  , ( "out of bounds"
    , Just 2
    , ( 2, "Joe", 0 )
    , ( 3, "Bob", 0 )
    , EGYtdWins
    )
  , ( "no goalie selected"
    , Nothing
    , ( 2, "Joe", 0 )
    , ( 3, "Bob", 0 )
    , EGYtdWins
    )
  ]

editGoalieYtdLossesSpec :: Spec
editGoalieYtdLossesSpec = describe "editGoalieYtdLosses" $ editTest
  (editGoalieYtdLosses 1)
  EGYtdLosses
  (\(num, name, losses) -> newGoalie num name & gYtd.gsLosses .~ losses)
  [ ( "set Joe"
    , Just 0
    , ( 2, "Joe", 1 )
    , ( 3, "Bob", 0 )
    , EGYtd
    )
  , ( "set Bob"
    , Just 1
    , ( 2, "Joe", 0 )
    , ( 3, "Bob", 1 )
    , EGYtd
    )
  , ( "out of bounds"
    , Just 2
    , ( 2, "Joe", 0 )
    , ( 3, "Bob", 0 )
    , EGYtdLosses
    )
  , ( "no goalie selected"
    , Nothing
    , ( 2, "Joe", 0 )
    , ( 3, "Bob", 0 )
    , EGYtdLosses
    )
  ]

editTest
  :: (ProgState -> ProgState)
  -> EditGoalieMode
  -> (a -> Goalie)
  -> [(String, Maybe Int, a, a, EditGoalieMode)]
  -> Spec
editTest func setMode mkGoalie params = do
  mapM_
    (\(setLabel, setGid, joeData, bobData, expectMode) -> context setLabel $ do
      let
        egs = newEditGoalieState
          & egsSelectedGoalie .~ setGid
          & egsMode           .~ setMode

        ps = func $ progState $ EditGoalie egs

      mapM_
        (\(chkLabel, chkGid, goalieData) -> context chkLabel $ let
          actual   = fromJust $ nth chkGid $ ps^.database.dbGoalies
          expected = mkGoalie goalieData
          in it ("should be " ++ show expected) $
            actual `shouldBe` expected)
        --  label,       goalie ID, goalie data
        [ ( "check Joe", 0,         joeData     )
        , ( "check Bob", 1,         bobData     )
        ]

      context "check mode" $
        it ("should be " ++ show expectMode) $
          ps^.progMode.editGoalieStateL.egsMode `shouldBe` expectMode)

    params

  context "wrong progMode" $ do
    let ps = func $ progState MainMenu

    it "should not change the database" $
      ps^.database `shouldBe` db

    it "should not change the progMode" $
      show (ps^.progMode) `shouldBe` "MainMenu"

joe :: Goalie
joe = newGoalie 2 "Joe"

bob :: Goalie
bob = newGoalie 3 "Bob"

db :: Database
db = newDatabase & dbGoalies .~ [joe, bob]

progState :: ProgMode -> ProgState
progState mode = newProgState
  & progMode .~ mode
  & database .~ db
