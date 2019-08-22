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

{-# LANGUAGE LambdaCase #-}

module Mtlstats.Actions
  ( startNewSeason
  , resetYtd
  , startNewGame
  , setHomeGame
  , setAwayGame
  ) where

import Lens.Micro (over, (&), (.~), (?~), (%~))

import Mtlstats.Types

-- | Starts a new season
startNewSeason :: ProgState -> ProgState
startNewSeason = (progMode .~ NewSeason) . (database . dbGames .~ 0)

-- | Resets all players year-to-date stats
resetYtd :: ProgState -> ProgState
resetYtd
  = (database . dbPlayers %~ map (pYtd .~ newPlayerStats))
  . (database . dbGoalies %~ map (gYtd .~ newGoalieStats))

-- | Starts a new game
startNewGame :: ProgState -> ProgState
startNewGame
  = (progMode .~ NewGame newGameState)
  . (database . dbGames .~ 0)

-- | Sets the game type to 'HomeGame'
setHomeGame :: ProgState -> ProgState
setHomeGame = over progMode $ \case
  NewGame gs -> NewGame (gs & gameType ?~ HomeGame)
  _ -> NewGame $ newGameState & gameType ?~ HomeGame

-- | Sets the game type to 'AwayGame'
setAwayGame :: ProgState -> ProgState
setAwayGame = over progMode $ \case
  NewGame gs -> NewGame (gs & gameType ?~ AwayGame)
  _ -> NewGame $ newGameState & gameType ?~ AwayGame
