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

module ReportSpec (spec) where

import Lens.Micro ((&), (?~))
import Test.Hspec (Spec, context, describe, it, shouldBe)

import Mtlstats.Report
import Mtlstats.Types

spec :: Spec
spec = describe "Mtlstats.Report" $ do
  gameDateSpec
  playerNameColWidthSpec

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

playerNameColWidthSpec :: Spec
playerNameColWidthSpec = describe "playerNameColWidth" $ do
  let
    short1 = newPlayer 1 "short" "foo"
    short2 = newPlayer 2 "shorty" "bar"
    long   = newPlayer 3 "123456789012345" "baz"

  mapM_
    (\(label, players, expected) -> context label $
      it ("should be " ++ show expected) $
        playerNameColWidth players `shouldBe` expected)
    --  label,         players,          expected
    [ ( "empty list",  [],               10       )
    , ( "short names", [short1, short2], 10       )
    , ( "long name",   [short1, long],   16       )
    ]
