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

module UtilSpec (spec) where

import Test.Hspec (Spec, context, describe, it, shouldBe)

import Mtlstats.Util

spec :: Spec
spec = describe "Mtlstats.Util"
  nthSpec

nthSpec :: Spec
nthSpec = describe "nth" $ mapM_
  (\(n, expected) -> context (show n) $
    it ("should be " ++ show expected) $ let
      xs = ["foo", "bar", "baz"]
      in nth n xs `shouldBe` expected)
  --  index, expected
  [ ( 0,     Just "foo" )
  , ( 1,     Just "bar" )
  , ( 2,     Just "baz" )
  , ( 3,     Nothing    )
  , ( -1,    Nothing    )
  ]
