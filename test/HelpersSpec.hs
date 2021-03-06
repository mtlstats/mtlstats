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

module HelpersSpec (spec) where

import Test.Hspec (Spec, describe)

import qualified Helpers.GoalieSpec as Goalie
import qualified Helpers.PlayerSpec as Player
import qualified Helpers.PositionSpec as Position

spec :: Spec
spec = describe "Helper" $ do
  Player.spec
  Goalie.spec
  Position.spec
