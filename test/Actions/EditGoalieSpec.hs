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
spec = describe "EditGoalie"
  setGoalieNumberSpec

setGoalieNumberSpec :: Spec
setGoalieNumberSpec = describe "setGoalieNumber" $ do
  let
    joe = newGoalie 2 "Joe"
    bob = newGoalie 3 "Bob"
    db  = newDatabase & dbGoalies .~ [joe, bob]

    progState m = newProgState
      & progMode .~ m
      & database .~ db
      & setGoalieNumber 5

  mapM_
    (\(setLabel, setGid, mode, joeData, bobData) -> context setLabel $ do
      let
        egs = newEditGoalieState
          & egsSelectedGoalie .~ setGid
          & egsMode           .~ EGNumber

        pm = EditGoalie egs
        ps = progState pm

      mapM_
        (\(chkLabel, chkGid, (number, name)) -> context chkLabel $ let
          g = fromJust $ nth chkGid $ ps^.database.dbGoalies

          in context "check goalie" $ let
            expected = newGoalie number name
            in it ("should be " ++ show expected) $
              g `shouldBe` expected)

        --  label,       goalie ID, data
        [ ( "check Joe", 0,         joeData )
        , ( "check Bob", 1,         bobData )
        ]

      context "check mode" $
        it ("should be " ++ show mode) $
          ps^.progMode.editGoalieStateL.egsMode `shouldBe` mode)

    [ ( "set Joe"
      , Just 0
      , EGMenu
      , (5, "Joe")
      , (3, "Bob")
      )
    , ( "set Bob"
      , Just 1
      , EGMenu
      , (2, "Joe")
      , (5, "Bob")
      )
    , ( "out of bounds"
      , Just 2
      , EGNumber
      , (2, "Joe")
      , (3, "Bob")
      )
    , ( "no goalie selected"
      , Nothing
      , EGNumber
      , (2, "Joe")
      , (3, "Bob")
      )
    ]

  context "wrong progMode" $ do
    let ps = progState MainMenu

    it "should not change the database" $
      ps^.database `shouldBe` db

    it "should not change the progMode" $
      show (ps^.progMode) `shouldBe` "MainMenu"
