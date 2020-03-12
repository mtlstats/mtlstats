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

module Mtlstats.Config where

-- | The name of the team whose stats we're tracking
myTeam :: String
myTeam = "MONTREAL"

-- | The maximum number of function keys
maxFunKeys :: Int
maxFunKeys = 9

-- | The application name
appName :: String
appName = "mtlstats"

-- | The maximum number of assists
maxAssists :: Int
maxAssists = 2

-- | The length of a typical game (in minutes)
gameLength :: Int
gameLength = 60

-- | Report output filename
reportFilename :: FilePath
reportFilename = "report.txt"

-- | Number of columns in report file
reportCols :: Int
reportCols = 79
