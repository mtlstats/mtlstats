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

module Types.MenuSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Mtlstats.Types.Menu

spec :: Spec
spec = describe "Mtlstats.Types.Menu"
  menuSpec

menuSpec :: Spec
menuSpec = describe "Menu"
  showSpec

showSpec :: Spec
showSpec = describe "show" $
  it "should display correctly" $ let
    menu = Menu "Foo" ()
      [ MenuItem '1' "foo" $ return ()
      , MenuItem '2' "bar baz" $ return ()
      ]
    expected =
      "Foo\n\n\
      \1: foo    \n\
      \2: bar baz\n"
    in show menu `shouldBe` expected
