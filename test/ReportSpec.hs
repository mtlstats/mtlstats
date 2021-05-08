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

module ReportSpec (spec) where

import Lens.Micro ((&), (?~))
import Test.Hspec (Spec, context, describe, it, shouldBe)

import Mtlstats.Report
import Mtlstats.Types

spec :: Spec
spec = describe "Mtlstats.Report"
  gameDateSpec

gameDateSpec :: Spec
gameDateSpec = describe "gameDate" $ do

  context "valid gameDate" $
    it "should format the date" $ let
      gs = newGameState
        & gameYear  ?~ 1980
        & gameMonth ?~ 6
        & gameDay   ?~ 25
      in gameDate gs `shouldBe` "JUN 25 1980"

  context "invalid date" $
    it "should return an empty string" $
      gameDate newGameState `shouldBe` ""
