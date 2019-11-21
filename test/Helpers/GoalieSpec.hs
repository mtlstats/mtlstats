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

module Helpers.GoalieSpec (spec) where

import Lens.Micro ((&), (.~), (%~))
import Test.Hspec (Spec, describe, it, shouldBe)

import Mtlstats.Helpers.Goalie
import Mtlstats.Types

spec :: Spec
spec = describe "Goalie"
  goalieDetailsSpec

goalieDetailsSpec :: Spec
goalieDetailsSpec = describe "goalieDetails" $ let
  input = newGoalie 1 "Joe"
    & gYtd
      %~ ( gsGames        .~ 2  )
      .  ( gsMinsPlayed   .~ 3  )
      .  ( gsGoalsAllowed .~ 4  )
      .  ( gsWins         .~ 5  )
      .  ( gsLosses       .~ 6  )
      .  ( gsTies         .~ 7  )
    & gLifetime
      %~ ( gsGames        .~ 8  )
      .  ( gsMinsPlayed   .~ 9  )
      .  ( gsGoalsAllowed .~ 10 )
      .  ( gsWins         .~ 11 )
      .  ( gsLosses       .~ 12 )
      .  ( gsTies         .~ 13 )

  expected = unlines
    [ "Number: 1"
    , "  Name: Joe"
    , ""
    , "              YTD Lifetime"
    , " Games played   2        8"
    , "  Mins played   3        9"
    , "Goals allowed   4       10"
    , "         Wins   5       11"
    , "       Losses   6       12"
    , "         Ties   7       13"
    ]

  in it "should format the output correctly" $
    goalieDetails input `shouldBe` expected
