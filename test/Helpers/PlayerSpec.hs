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

module Helpers.PlayerSpec (spec) where

import Lens.Micro ((&), (.~))
import Test.Hspec (Spec, context, describe, it, shouldBe)

import Mtlstats.Helpers.Player
import Mtlstats.Types

spec :: Spec
spec = describe "Player" $ do
  playerDetailsSpec
  playerNameSpec

playerDetailsSpec :: Spec
playerDetailsSpec = describe "playerDetails" $
  it "should give a detailed description" $ let

    p = newPlayer 1 "Joe" "centre"
      & pRookie .~ True
      & pYtd .~ PlayerStats
        { _psGoals   = 2
        , _psAssists = 3
        , _psPMin    = 4
        }
      & pLifetime .~ PlayerStats
        { _psGoals   = 5
        , _psAssists = 6
        , _psPMin    = 7
        }

    expected = unlines
      [ "  Number: 1"
      , "    Name: Joe*"
      , "Position: centre"
      , ""
      , "             YTD Lifetime"
      , "       Goals   2        5"
      , "     Assists   3        6"
      , "Penalty mins   4        7"
      ]

    in playerDetails p `shouldBe` expected

playerNameSpec :: Spec
playerNameSpec = describe "playerName" $ mapM_
  (\(label, p, expected) -> context label $
    it ("should be " ++ expected) $
      playerName p `shouldBe` expected)

  --  label,        player,    expected
  [ ( "rookie",     rookie,    "foo*"   )
  , ( "non-rookie", nonRookie, "foo"    )
  ]

  where
    rookie    = player True
    nonRookie = player False
    player r  = newPlayer 1 "foo" "centre" & pRookie .~ r
