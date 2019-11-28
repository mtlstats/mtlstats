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

module Mtlstats.Report (report, gameDate, playerNameColWidth) where

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
  -> [String]
report width s
  =  standingsReport width s
  ++ [""]
  ++ gameStatsReport width s
  ++ [""]
  ++ yearToDateStatsReport width s
  ++ [""]
  ++ lifetimeStatsReport width s

standingsReport :: Int -> ProgState -> [String]
standingsReport width s = fromMaybe [] $ do
  let
    db     = s^.database
    gs     = s^.progMode.gameStateL
    gNum   = db^.dbGames
    date   = gameDate gs
    hTeam  = homeTeam gs
    aTeam  = awayTeam gs
    hStats = db^.dbHomeGameStats
    aStats = db^.dbAwayGameStats
    tStats = addGameStats hStats aStats
  hScore <- gs^.homeScore
  aScore <- gs^.awayScore
  Just
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
    ]

gameStatsReport :: Int -> ProgState -> [String]
gameStatsReport width s = playerReport width "GAME" $
  fromMaybe [] $ mapM
    (\(pid, stats) -> do
      p <- nth pid $ s^.database.dbPlayers
      Just (p, stats))
    (M.toList $ s^.progMode.gameStateL.gamePlayerStats)

yearToDateStatsReport :: Int -> ProgState -> [String]
yearToDateStatsReport width s = playerReport width "YEAR TO DATE" $
  map (\p -> (p, p^.pYtd)) $
  filter playerIsActive $ s^.database.dbPlayers

lifetimeStatsReport :: Int -> ProgState -> [String]
lifetimeStatsReport width s = playerReport width "LIFETIME" $
  map (\p -> (p, p^.pLifetime)) $ s^.database.dbPlayers

gameDate :: GameState -> String
gameDate gs = fromMaybe "" $ do
  y <- show <$> gs^.gameYear
  m <- month <$> gs^.gameMonth
  d <- padNum 2 <$> gs^.gameDay
  Just $ m ++ " " ++ d ++ " " ++ y

playerReport :: Int -> String -> [(Player, PlayerStats)] -> [String]
playerReport width label ps = let
  tStats = foldl addPlayerStats newPlayerStats $ map snd ps

  rHeader =
    [ centre width (label ++ " STATISTICS")
    , ""
    ]

  tHeader =
    [ CellText "NO."
    , CellText "Player"
    , CellText "   G"
    , CellText "   A"
    , CellText "   P"
    , CellText "  PM"
    ]

  statsCells stats =
    [ CellText $ show $ stats^.psGoals
    , CellText $ show $ stats^.psAssists
    , CellText $ show $ psPoints stats
    , CellText $ show $ stats^.psPMin
    ]

  body = map
    (\(p, stats) ->
      [ CellText $ show (p^.pNumber) ++ " "
      , CellText $ p^.pName
      ] ++ statsCells stats)
    ps

  separator = replicate 2 (CellText "") ++ replicate 4 (CellFill '-')

  totals =
    [ CellText ""
    , CellText ""
    ] ++ statsCells tStats

  table = overlayLast (label ++ " TOTALS")
    $ map (centre width)
    $ complexTable ([right, left] ++ repeat right)
    $ tHeader : body ++ [separator, totals]

  in rHeader ++ table

playerNameColWidth :: [Player] -> Int
playerNameColWidth = foldr
  (\player current -> max current $ succ $ length $ player^.pName)
  10

showStats :: GameStats -> String
showStats gs
  =  right 2 (show $ gmsGames gs)
  ++ right 4 (show $ gs^.gmsWins)
  ++ right 4 (show $ gs^.gmsLosses)
  ++ right 4 (show $ gs^.gmsOvertime)
  ++ right 4 (show $ gs^.gmsGoalsFor)
  ++ right 4 (show $ gs^.gmsGoalsAgainst)
  ++ right 4 (show $ gmsPoints gs)
