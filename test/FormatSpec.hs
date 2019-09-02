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

module FormatSpec (spec) where

import Test.Hspec (Spec, context, describe, it, shouldBe)

import Mtlstats.Format

spec :: Spec
spec = describe "Mtlstats.Format"
  padNumSpec

padNumSpec :: Spec
padNumSpec = describe "padNum" $ do

  context "zero, four digits" $
    it "should be 0000" $
      padNum 4 0 `shouldBe` "0000"

  context "123, four digits" $
    it "should be 0123" $
      padNum 4 123 `shouldBe` "0123"

  context "12345, four digits" $
    it "should be 12345" $
      padNum 4 12345 `shouldBe` "12345"

  context "-12, four digits" $
    it "should be -012" $
      padNum 4 (-12) `shouldBe` "-012"

  context "-1234, four digits" $
    it "should be -1234" $
      padNum 4 (-1234) `shouldBe` "-1234"
