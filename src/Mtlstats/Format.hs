{- |

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

module Mtlstats.Format
  ( padNum
  , left
  , right
  ) where

-- | Pad an 'Int' with leading zeroes to fit a certain character width
padNum
  :: Int
  -- ^ The width in characters
  -> Int
  -- ^ The value to format
  -> String
padNum size n
  | n < 0 = '-' : padNum (pred size) (-n)
  | otherwise = let
    str  = show n
    sLen = length str
    pLen = size - sLen
    pad  = replicate pLen '0'
    in pad ++ str

-- | Aligns text to the left within a field (clipping if necessary)
left
  :: Int
  -- ^ The width of the field
  -> String
  -- ^ The text to align
  -> String
left n str = take n $ str ++ repeat ' '

-- | Aligns text to the right within a field (clipping if necessary)
right
  :: Int
  -- ^ The width of the field
  -> String
  -- ^ The text to align
  -> String
right n str = reverse $ left n $ reverse str
