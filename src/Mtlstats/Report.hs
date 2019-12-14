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
import Data.Maybe (fromMaybe, mapMaybe)
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
  let
    rHeader =
      [ overlay
        ("GAME NUMBER " ++ padNum 2 gNum)
        (centre width
          $  aTeam ++ " " ++ show aScore ++ "  AT  "
          ++ hTeam ++ " " ++ show hScore)
      , date
      , centre width "STANDINGS"
      , ""
      ]

    tHeader =
      [ CellText myTeam
      , CellText " G"
      , CellText "   W"
      , CellText "   L"
      , CellText "  OT"
      , CellText "  GF"
      , CellText "  GA"
      , CellText "   P"
      ]

    rowCells stats =
      [ CellText $ show $ gmsGames stats
      , CellText $ show $ stats^.gmsWins
      , CellText $ show $ stats^.gmsLosses
      , CellText $ show $ stats^.gmsOvertime
      , CellText $ show $ stats^.gmsGoalsFor
      , CellText $ show $ stats^.gmsGoalsAgainst
      , CellText $ show $ gmsPoints stats
      ]

    body =
      [ CellText "HOME" : rowCells hStats
      , CellText "ROAD" : rowCells aStats
      ]

    separator = CellText "" : replicate 7 (CellFill '-')
    totals = CellText "TOTALS" : rowCells tStats

    table = map (centre width) $
      complexTable
      (left : repeat right)
      (tHeader : body ++ [separator, totals])

  Just $ rHeader ++ table

gameStatsReport :: Int -> ProgState -> [String]
gameStatsReport width s = let
  gs = s^.progMode.gameStateL
  db = s^.database

  playerStats = mapMaybe
    (\(pid, stats) -> do
      p <- nth pid $ db^.dbPlayers
      Just (p, stats))
    (M.toList $ gs^.gamePlayerStats)

  goalieStats = mapMaybe
    (\(gid, stats) -> do
      g <- nth gid $ db^.dbGoalies
      Just (g, stats))
    (M.toList $ gs^.gameGoalieStats)

  criteria (_, ps) = psPoints ps > 0

  in filteredPlayerReport width "GAME" criteria playerStats
  ++ [""]
  ++ gameGoalieReport width goalieStats

yearToDateStatsReport :: Int -> ProgState -> [String]
yearToDateStatsReport width s = let
  db = s^.database

  playerStats = map (\p -> (p, p^.pYtd))
    $ filter playerIsActive
    $ db^.dbPlayers

  goalieStats = map (\g -> (g, g^.gYtd))
    $ filter goalieIsActive
    $ db^.dbGoalies

  in playerReport width "YEAR TO DATE" playerStats
  ++ [""]
  ++ goalieReport width goalieStats

lifetimeStatsReport :: Int -> ProgState -> [String]
lifetimeStatsReport width s = let
  db = s^.database

  playerStats = map (\p -> (p, p^.pLifetime))
    $ db^.dbPlayers

  goalieStats = map (\g -> (g, g^.gLifetime))
    $ db^.dbGoalies

  in playerReport width "LIFETIME" playerStats
  ++ [""]
  ++ goalieReport width goalieStats

gameDate :: GameState -> String
gameDate gs = fromMaybe "" $ do
  y <- show <$> gs^.gameYear
  m <- month <$> gs^.gameMonth
  d <- padNum 2 <$> gs^.gameDay
  Just $ m ++ " " ++ d ++ " " ++ y

playerReport :: Int -> String -> [(Player, PlayerStats)] -> [String]
playerReport width label =
  filteredPlayerReport width label (const True)

filteredPlayerReport
  :: Int
  -> String
  -> ((Player, PlayerStats) -> Bool)
  -> [(Player, PlayerStats)]
  -> [String]
filteredPlayerReport width label criteria ps = let
  tStats = foldl addPlayerStats newPlayerStats $ map snd ps
  fps    = filter criteria ps

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
    fps

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

goalieReport :: Int -> [(Goalie, GoalieStats)] -> [String]
goalieReport width goalieData = let
  olayText = "GOALTENDING TOTALS"

  tData = foldl addGoalieStats newGoalieStats
    $ map snd goalieData

  header =
    [ CellText "NO."
    , CellText $ left (length olayText) "GOALTENDER"
    , CellText "GP"
    , CellText " MIN"
    , CellText "  GA"
    , CellText "  SO"
    , CellText "AVE"
    ]

  rowCells stats =
    [ CellText $ show $ stats^.gsGames
    , CellText $ show $ stats^.gsMinsPlayed
    , CellText $ show $ stats^.gsGoalsAllowed
    , CellText $ show $ stats^.gsShutouts
    , CellText $ showFloating $ gsAverage stats
    ]

  body = map
    (\(goalie, stats) ->
      [ CellText $ show (goalie^.gNumber) ++ " "
      , CellText $ goalie^.gName
      ] ++ rowCells stats)
    goalieData

  separator
    =  replicate 2 (CellText "")
    ++ replicate 5 (CellFill '-')

  summary = replicate 2 (CellText  "") ++ rowCells tData

  in map (centre width)
    $ overlayLast olayText
    $ complexTable ([right, left] ++ repeat right)
    $ header : body ++ [separator, summary]

gameGoalieReport :: Int -> [(Goalie, GoalieStats)] -> [String]
gameGoalieReport width goalieData = let
  header =
    [ CellText "NO."
    , CellText "GOALTENDER"
    , CellText "   MIN"
    , CellText "    GA"
    , CellText "   AVE"
    ]

  body = map
    (\(goalie, stats) ->
      [ CellText $ show (goalie^.gNumber) ++ " "
      , CellText $ goalie^.gName
      , CellText $ show $ stats^.gsMinsPlayed
      , CellText $ show $ stats^.gsGoalsAllowed
      , CellText $ showFloating $ gsAverage stats
      ])
    goalieData

  in map (centre width)
    $ complexTable ([right, left] ++ repeat right)
    $ header : body
