{- |

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

{-# LANGUAGE LambdaCase #-}

module Mtlstats.Helpers.Position
  ( posSearch
  , posSearchExact
  , posCallback
  , getPositions
  ) where

import Control.Monad.Trans.State (gets)
import Data.Char (toUpper)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Lens.Micro ((^.), to)

import Mtlstats.Types
import Mtlstats.Util

-- | Searches the 'Database' for all the positions used
posSearch
  :: String
  -- ^ The search string
  -> Database
  -- ^ The database
  -> [(Int, String)]
  -- ^ A list of result indices and their values
posSearch sStr db = filter sFunc $ zip [0..] ps
  where
    sFunc (_, pos) = map toUpper sStr `isInfixOf` map toUpper pos
    ps = getPositions db

-- | Searches the 'Database' for an exact position
posSearchExact
  :: String
  -- ^ The search string
  -> Database
  -- ^ The database
  -> Maybe Int
  -- ^ The index of the result (or 'Nothing' if not found)
posSearchExact sStr db = case filter sFunc $ zip [0..] ps of
  []      -> Nothing
  (n,_):_ -> Just n
  where
    sFunc (_, pos) = sStr == pos
    ps = getPositions db

-- | Builds a callback function for when a 'Player' position is
-- selected
posCallback
  :: (String -> Action ())
  -- ^ The raw callback function
  -> Maybe Int
  -- ^ The index number of the position selected or 'Nothing' if blank
  -> Action ()
  -- ^ The action to perform
posCallback callback = \case
  Nothing -> callback ""
  Just n  -> do
    ps <- gets (^.database.to getPositions)
    let pos = fromMaybe "" $ nth n ps
    callback pos

-- | Extracts a list of positions from a 'Database'
getPositions :: Database -> [String]
getPositions = do
  raw <- map (^.pPosition) . (^.dbPlayers)
  return $ S.toList $ S.fromList raw
