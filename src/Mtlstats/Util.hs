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

module Mtlstats.Util
  ( nth
  , modifyNth
  , updateMap
  , slice
  , capitalizeName
  ) where

import Data.Char (isSpace, toUpper)
import qualified Data.Map as M

-- | Attempt to select the element from a list at a given index
nth
  :: Int
  -- ^ The index
  -> [a]
  -- ^ The list
  -> Maybe a
nth _ [] = Nothing
nth n (x:xs)
  | n == 0    = Just x
  | n < 0     = Nothing
  | otherwise = nth (pred n) xs

-- | Attempt to modify the index at a given index in a list
modifyNth
  :: Int
  -- ^ The index
  -> (a -> a)
  -- ^ The modification function
  -> [a]
  -- ^ The list
  -> [a]
modifyNth n f = map (\(i, x) -> if i == n then f x else x)
  . zip [0..]

-- | Modify a value indexed by a given key in a map using a default
-- initial value if not present
updateMap
  :: Ord k
  => k
  -- ^ The key
  -> a
  -- ^ The default initial value
  -> (a -> a)
  -- ^ The modification function
  -> M.Map k a
  -- ^ The map
  -> M.Map k a
updateMap k def f m = let
  x = M.findWithDefault def k m
  in M.insert k (f x) m

-- | Selects a section of a list
slice
  :: Int
  -- ^ The index to start at
  -> Int
  -- ^ The number of elements to take
  -> [a]
  -- ^ The list to take a subset of
  -> [a]
slice offset len = take len . drop offset

-- | Name capitalization function for a player
capitalizeName
  :: Char
  -- ^ The character being input
  -> String
  -- ^ The current string
  -> String
  -- ^ The resulting string
capitalizeName ch str = str ++ [ch']
  where
    ch' = if lockFlag str
      then toUpper ch
      else ch
    lockFlag "" = True
    lockFlag (c:cs)
      | c == ','  = lockFlag' cs
      | otherwise = lockFlag cs
    lockFlag' "" = True
    lockFlag' (c:cs)
      | isSpace c = lockFlag' cs
      | otherwise = False
