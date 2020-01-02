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
      .  ( gsShutouts     .~ 5  )
      .  ( gsWins         .~ 6  )
      .  ( gsLosses       .~ 7  )
      .  ( gsTies         .~ 8  )
    & gLifetime
      %~ ( gsGames        .~ 9  )
      .  ( gsMinsPlayed   .~ 10 )
      .  ( gsGoalsAllowed .~ 11 )
      .  ( gsShutouts     .~ 12 )
      .  ( gsWins         .~ 13 )
      .  ( gsLosses       .~ 14 )
      .  ( gsTies         .~ 15 )

  expected = unlines
    [ "Number: 1"
    , "  Name: Joe"
    , ""
    , "              YTD Lifetime"
    , " Games played   2        9"
    , "  Mins played   3       10"
    , "Goals allowed   4       11"
    , "     Shutouts   5       12"
    , "         Wins   6       13"
    , "       Losses   7       14"
    , "         Ties   8       15"
    ]

  in it "should format the output correctly" $
    goalieDetails input `shouldBe` expected
