{- |

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

module Mtlstats.Handlers (ynHandler) where

import Data.Char (toUpper)
import qualified UI.NCurses as C

-- | Handler for a yes/no prompt
ynHandler :: C.Event -> Maybe Bool
ynHandler (C.EventCharacter c) = case toUpper c of
  'Y' -> Just True
  'N' -> Just False
  _   -> Nothing
ynHandler _ = Nothing
