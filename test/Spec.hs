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

import Test.Hspec (hspec)

import qualified ActionsSpec as Actions
import qualified FormatSpec as Format
import qualified HandlersSpec as Handlers
import qualified HelpersSpec as Helpers
import qualified ReportSpec as Report
import qualified TypesSpec as Types
import qualified UtilSpec as Util

main :: IO ()
main = hspec $ do
  Types.spec
  Helpers.spec
  Actions.spec
  Format.spec
  Handlers.spec
  Report.spec
  Util.spec
