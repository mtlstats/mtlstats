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

module ActionsSpec (spec) where

import Lens.Micro ((&), (.~), (^.))
import Test.Hspec (Spec, describe, it, shouldBe)

import Mtlstats.Actions
import Mtlstats.Types

spec :: Spec
spec = describe "Mtlstats.Actions" $ do
  startNewSeasonSpec
  startNewGameSpec

startNewSeasonSpec :: Spec
startNewSeasonSpec = describe "startNewSeason" $ do
  let
    s = newProgState
      & database . dbGames .~ 1
      & startNewSeason

  it "should set the progState to NewSeason" $
    s ^. progMode `shouldBe` NewSeason

  it "should set the number of games to 0" $
    s ^. database . dbGames `shouldBe` 0

startNewGameSpec :: Spec
startNewGameSpec = describe "startGame" $ return ()
