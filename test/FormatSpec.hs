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
spec = describe "Mtlstats.Format" $ do
  padNumSpec
  leftSpec
  rightSpec
  centreSpec
  overlaySpec
  monthSpec

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

leftSpec :: Spec
leftSpec = describe "left" $ do

  context "fit" $
    it "should pad the text" $
      left 5 "foo" `shouldBe` "foo  "

  context "overflow" $
    it "should truncate the text" $
      left 2 "foo" `shouldBe` "fo"

rightSpec :: Spec
rightSpec = describe "right" $ do

  context "fit" $
    it "should pad the text" $
      right 5 "foo" `shouldBe` "  foo"

  context "overflow" $
    it "should truncate the text" $
      right 2 "foo" `shouldBe` "oo"

centreSpec :: Spec
centreSpec = describe "centre" $ do

  context "fit" $
    it "should pad the text" $
      centre 5 "foo" `shouldBe` " foo "

  context "overflow" $
    it "should truncate the text" $
      centre 2 "foo" `shouldBe` "fo"

overlaySpec :: Spec
overlaySpec = describe "overlay" $ do

  context "first string shorter" $
    it "should overlay" $
      overlay "foo" "abc123" `shouldBe` "foo123"

  context "first string longer" $
    it "should overlay" $
      overlay "abc123" "foo" `shouldBe` "abc123"

monthSpec :: Spec
monthSpec = describe "month" $ do

  context "January" $
    it "should return \"JAN\"" $
      month 1 `shouldBe` "JAN"

  context "invalid" $
    it "should return an empty string" $
      month 0 `shouldBe` ""
