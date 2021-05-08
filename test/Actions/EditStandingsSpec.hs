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

{-# LANGUAGE LambdaCase #-}

module Actions.EditStandingsSpec (spec) where

import Lens.Micro ((^.), (&), (.~))
import Test.Hspec
  ( Spec
  , context
  , describe
  , it
  , shouldBe
  , shouldSatisfy
  )

import Mtlstats.Actions.EditStandings
import Mtlstats.Types

spec :: Spec
spec = describe "EditStandings" $ do
  mapM_
    (\(label, f, expected) -> describe label $ do
      let
        ps  = newProgState
        ps' = f ps

      it "should set progMode to EditStandings" $
        ps'^.progMode `shouldSatisfy` \case
          (EditStandings _) -> True
          _                 -> False

      it ("should set editStandingsMode to " ++ show expected) $
        ps'^.progMode.editStandingsModeL `shouldBe` expected)

    --  label,               function,          expected mode
    [ ( "editStandings",     editStandings,     ESMMenu            )
    , ( "editHomeStandings", editHomeStandings, ESMHome ESMSubMenu )
    , ( "editAwayStandings", editAwayStandings, ESMAway ESMSubMenu )
    ]

  mapM_
    (\(label, f, expected) -> describe label $ do
      mapM_
        (\prefix -> context ("mode: " ++ show (prefix ESMSubMenu)) $ let
          ps  = newProgState & progMode.editStandingsModeL .~ prefix ESMSubMenu
          ps' = f ps
          in it ("should set the mode to " ++ show expected) $
            ps'^.progMode.editStandingsModeL `shouldBe` prefix expected)
        [ESMHome, ESMAway]

      context "mode: ESMMenu" $ let
        ps  = newProgState & progMode.editStandingsModeL .~ ESMMenu
        ps' = f ps
        in it "should not change the mode" $
          ps'^.progMode.editStandingsModeL `shouldBe` ESMMenu)

    --  label,              function,         expected
    [ ( "editWins",         editWins,         ESMEditWins         )
    , ( "editLosses",       editLosses,       ESMEditLosses       )
    , ( "editOvertime",     editOvertime,     ESMEditOvertime     )
    , ( "editGoalsFor",     editGoalsFor,     ESMEditGoalsFor     )
    , ( "editGoalsAgainst", editGoalsAgainst, ESMEditGoalsAgainst )
    ]
