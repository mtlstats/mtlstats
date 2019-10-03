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

module Mtlstats.Report (report, gameDate) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Lens.Micro ((^.))

import Mtlstats.Config
import Mtlstats.Format
import Mtlstats.Types
import Mtlstats.Util

-- | Generates the report
report
  :: Int
  -- ^ The number of columns for the report
  -> ProgState
  -- ^ The program state
  -> String
report width s = unlines $ fromMaybe [] $ do
  let
    db      = s^.database
    gs      = s^.progMode.gameStateL
    gNum    = db^.dbGames
    date    = gameDate gs
    hTeam   = homeTeam gs
    aTeam   = awayTeam gs
    hStats  = db^.dbHomeGameStats
    aStats  = db^.dbAwayGameStats
    tStats  = addGameStats hStats aStats
    players = db^.dbPlayers
  hScore <- gs^.homeScore
  aScore <- gs^.awayScore
  pStats <- mapM
    (\(n, stats) -> do
      player <- nth n players
      Just (player, stats))
    (M.toList $ gs^.gamePlayerStats)
  Just $
    [ overlay
      ("GAME NUMBER " ++ padNum 2 gNum)
      (centre width
        $  aTeam ++ " " ++ show aScore ++ "  AT  "
        ++ hTeam ++ " " ++ show hScore)
    , date
    , centre width "STANDINGS"
    , ""
    , centre width
      $  left 11 myTeam
      ++ right 2 "G"
      ++ right 4 "W"
      ++ right 4 "L"
      ++ right 4 "OT"
      ++ right 4 "GF"
      ++ right 4 "GA"
      ++ right 4 "P"
    , centre width
      $  left 11 "HOME"
      ++ showStats hStats
    , centre width
      $ left 11 "ROAD"
      ++ showStats aStats
    , centre width
      $  replicate 11 ' '
      ++ replicate (2 + 4 * 6) '-'
    , centre width
      $  left 11 "TOTALS"
      ++ showStats tStats
    , ""
    , centre width "GAME STATISTICS"
    , ""
    , centre width
      $  "NO. "
      ++ left 20 "PLAYER"
      ++ right 3 "G"
      ++ right 6 "A"
      ++ right 6 "P"
      ++ right 6 "PM"
    ] ++ map
      (\(p, stats) -> centre width
        $  right 2 (show $ p^.pNumber)
        ++ "  "
        ++ left 20 (p^.pName)
        ++ right 3 (show $ stats^.psGoals)
        ++ right 6 (show $ stats^.psAssists)
        ++ right 6 (show $ pPoints stats)
        ++ right 6 (show $ stats^.psPMin))
      pStats

gameDate :: GameState -> String
gameDate gs = fromMaybe "" $ do
  year  <- show <$> gs^.gameYear
  month <- month <$> gs^.gameMonth
  day   <- padNum 2 <$> gs^.gameDay
  Just $ month ++ " " ++ day ++ " " ++ year

showStats :: GameStats -> String
showStats gs
  =  right 2 (show $ gmsGames gs)
  ++ right 4 (show $ gs^.gmsWins)
  ++ right 4 (show $ gs^.gmsLosses)
  ++ right 4 (show $ gs^.gmsOvertime)
  ++ right 4 (show $ gs^.gmsGoalsFor)
  ++ right 4 (show $ gs^.gmsGoalsAgainst)
  ++ right 4 (show $ gmsPoints gs)
