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
  , centre
  , overlay
  , month
  , labelTable
  , numTable
  , tableWith
  ) where

import Data.List (transpose)

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

-- | Aligns text to the centre within a field (clipping if necessary)
centre
  :: Int
  -- ^ The width of the field
  -> String
  -- ^ The text to align
  -> String
centre n str = let
  sLen = length str
  pLen = (n - sLen) `div` 2
  pad  = replicate pLen ' '
  in take n $ pad ++ str ++ repeat ' '

-- | Overlays one string on top of another
overlay
  :: String
  -- ^ The string on the top
  -> String
  -- ^ The string on the bottom
  -> String
overlay (x:xs) (_:ys) = x : overlay xs ys
overlay xs []         = xs
overlay [] ys         = ys

-- | Converts a number to a three character month (e.g. @"JAN"@)
month :: Int -> String
month 1  = "JAN"
month 2  = "FEB"
month 3  = "MAR"
month 4  = "APR"
month 5  = "MAY"
month 6  = "JUN"
month 7  = "JUL"
month 8  = "AUG"
month 9  = "SEP"
month 10 = "OCT"
month 11 = "NOV"
month 12 = "DEC"
month _  = ""

-- | Creates a two-column table with labels
labelTable :: [(String, String)] -> [String]
labelTable xs = let
  labelWidth = maximum $ map (length . fst) xs
  in map
    (\(label, val) -> right labelWidth label ++ ": " ++ val)
    xs

-- | Creates a variable column table of numbers with two axes
numTable
  :: [String]
  -- ^ The top column labels
  -> [(String, [Int])]
  -- ^ The rows with their labels
  -> [String]
numTable headers rows = tableWith right $ header : body
  where
    header = "" : headers
    body   = map
      (\(label, row) ->
        label : map show row)
      rows

-- | Creates a table from a two-dimensional list with a specified
-- padding function
tableWith
  :: (Int -> String -> String)
  -- ^ The padding function
  -> [[String]]
  -- ^ The cells
  -> [String]
tableWith func tdata = let
  widths    = map (map length) tdata
  colWidths = map maximum $ transpose widths
  fitted    = map
    (\row -> map
      (\(str, len) -> func len str) $
      zip row colWidths)
    tdata
  in map unwords fitted
