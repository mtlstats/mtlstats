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

module UtilSpec (spec) where

import qualified Data.Map as M
import Test.Hspec (Spec, context, describe, it, shouldBe)

import Mtlstats.Util

spec :: Spec
spec = describe "Mtlstats.Util" $ do
  nthSpec
  modifyNthSpec
  dropNthSpec
  updateMapSpec
  sliceSpec
  capitalizeNameSpec

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

modifyNthSpec  :: Spec
modifyNthSpec = describe "modifyNth" $ do
  let list = [1, 2, 3] :: [Int]

  context "in bounds" $
    it "should modify the value" $
      modifyNth 1 succ list `shouldBe` [1, 3, 3]

  context "out of bounds" $
    it "should not modify the value" $
      modifyNth 3 succ list `shouldBe` [1, 2, 3]

  context "negative index" $
    it "should not modify the value" $
      modifyNth (-1) succ list `shouldBe` [1, 2, 3]

dropNthSpec :: Spec
dropNthSpec = describe "dropNth" $ mapM_

  (\(label, n, expected) ->
    context label $
      it ("should be " ++ show expected) $
      dropNth n list `shouldBe` expected)

  [ ( "out of bounds", 1, ["foo", "baz"] )
  , ( "in bounds",     3, list           )
  ]

  where list = ["foo", "bar", "baz"]

updateMapSpec :: Spec
updateMapSpec = describe "updateMap" $ do
  let
    input = M.fromList [(1, 2), (3, 5)]

  context "key found" $ let
    expected = M.fromList [(1, 3), (3, 5)] :: M.Map Int Int
    in it "should update the value" $
      updateMap 1 10 succ input `shouldBe` expected

  context "key not found" $ let
    expected = M.fromList [(1, 2), (3, 5), (10, 11)]
    in it "should create a new value and update the default" $
      updateMap 10 10 succ input `shouldBe` expected

sliceSpec :: Spec
sliceSpec = describe "slice" $ do
  let list = [2, 4, 6, 8] :: [Int]

  context "sublist" $
    it "should return the sublist" $
      slice 1 2 list `shouldBe` [4, 6]

  context "too large" $
    it "should return as much of the list as possible" $
      slice 1 100 list `shouldBe` [4, 6, 8]

  context "negative offset" $
    it "should return the correct number of elements from the beginning" $
      slice (-10) 2 list `shouldBe` [2, 4]

capitalizeNameSpec :: Spec
capitalizeNameSpec = describe "capitalizeName" $ mapM_
  (\(label, ch, str, expected) -> context label $
    it ("should be " ++ expected) $
      capitalizeName ch str `shouldBe` expected)
  --  label,                        character, string,   expected
  [ ( "initial lower",              'a',       "",       "A"       )
  , ( "initial upper",              'A',       "",       "A"       )
  , ( "initial non-alpha",          '0',       "",       "0"       )
  , ( "pre-comma lower",            'a',       "A",      "AA"      )
  , ( "pre-comma upper",            'A',       "A",      "AA"      )
  , ( "pre-comma non-alpha",        '0',       "A",      "A0"      )
  , ( "post-comma first lower",     'a',       "FOO, ",  "FOO, A"  )
  , ( "post-comma first upper",     'A',       "FOO, ",  "FOO, A"  )
  , ( "post-comma first non-alpha", '0',       "FOO, ",  "FOO, 0"  )
  , ( "unrestricted upper",         'A',       "FOO, A", "FOO, AA" )
  , ( "unrestricted lower",         'a',       "FOO, A", "FOO, Aa" )
  , ( "unrestricted non-alpha",     '0',       "FOO, A", "FOO, A0" )
  ]
