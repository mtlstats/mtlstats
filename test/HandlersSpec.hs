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

module HandlersSpec (spec) where

import Test.Hspec (Spec, context, describe, it, shouldBe)
import qualified UI.NCurses as C

import Mtlstats.Handlers

spec :: Spec
spec = describe "Mtlstats.Handlers"
  ynHandlerSpec

ynHandlerSpec :: Spec
ynHandlerSpec = describe "ynHandler" $ mapM_
  (\(desc, event, expected) ->
    context desc $
      it ("should be " ++ show expected) $
        ynHandler event `shouldBe` expected)
  --  description,   event,                expected
  [ ( "Y pressed",   C.EventCharacter 'Y', Just True  )
  , ( "y pressed",   C.EventCharacter 'y', Just True  )
  , ( "N pressed",   C.EventCharacter 'N', Just False )
  , ( "n pressed",   C.EventCharacter 'n', Just False )
  , ( "x pressed",   C.EventCharacter 'x', Nothing    )
  , ( "other event", C.EventResized,       Nothing    )
  ]
