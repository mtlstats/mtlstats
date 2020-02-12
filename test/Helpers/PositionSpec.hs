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

module Helpers.PositionSpec (spec) where

import Lens.Micro ((&), (.~))
import Test.Hspec (Spec, context, describe, it, shouldBe)

import Mtlstats.Helpers.Position
import Mtlstats.Types

spec :: Spec
spec = describe "Position" $ do
  posSearchSpec
  getPositionsSpec

posSearchSpec :: Spec
posSearchSpec = describe "posSearch" $ mapM_
  (\(sStr, expected) -> context ("search string: " ++ show sStr) $
    it ("should be " ++ show expected) $
      posSearch sStr db `shouldBe` expected)
  [ ( "fOo"
    , [ ( 2, "foo" )
      ]
    )
  , ( "A"
    , [ ( 0, "bar" )
      , ( 1, "baz" )
      ]
    )
  ]

getPositionsSpec :: Spec
getPositionsSpec = describe "getPositions" $ let
  expected = ["bar", "baz", "foo"]
  in it ("should be " ++ show expected) $
    getPositions db `shouldBe` expected

db :: Database
db = newDatabase & dbPlayers .~
  [ newPlayer 2 "Joe"  "foo"
  , newPlayer 3 "Bob"  "bar"
  , newPlayer 5 "Bill" "foo"
  , newPlayer 8 "Ed"   "baz"
  ]
