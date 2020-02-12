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

module Mtlstats.Helpers.Position
  ( posSearch
  , posSearchExact
  , posCallback
  ) where

import Mtlstats.Types

-- | Searches the 'Database' for all the positions used
posSearch
  :: String
  -- ^ The search string
  -> Database
  -- ^ The database
  -> [(Int, String)]
  -- ^ A list of result indices and their values
posSearch = undefined

-- | Searches the 'Database' for an exact position
posSearchExact
  :: String
  -- ^ The search string
  -> Database
  -- ^ The database
  -> Maybe Int
  -- ^ The index of the result (or 'Nothing' if not found)
posSearchExact = undefined

-- | Builds a callback function for when a 'Player' position is
-- selected
posCallback
  :: (String -> Action ())
  -- ^ The raw callback function
  -> Maybe Int
  -- ^ The index number of the position selected or 'Nothing' if blank
  -> Action ()
  -- ^ The action to perform
posCallback = undefined
