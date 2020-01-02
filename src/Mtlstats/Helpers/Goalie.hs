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

module Mtlstats.Helpers.Goalie (goalieDetails) where

import Lens.Micro ((^.))

import Mtlstats.Format
import Mtlstats.Types

-- | Provides a detailed 'String' describing a 'Goalie'
goalieDetails :: Goalie -> String
goalieDetails g = let
  header = unlines $ labelTable
    [ ( "Number", show $ g^.gNumber )
    , ( "Name",   g^.gName          )
    ]

  body = unlines $ numTable ["YTD", "Lifetime"] $ map
    (\(label, lens) -> (label, [g^.gYtd.lens, g^.gLifetime.lens]))
    [ ( "Games played",  gsGames        )
    , ( "Mins played",   gsMinsPlayed   )
    , ( "Goals allowed", gsGoalsAllowed )
    , ( "Shutouts",      gsShutouts     )
    , ( "Wins",          gsWins         )
    , ( "Losses",        gsLosses       )
    , ( "Ties",          gsTies         )
    ]

  in header ++ "\n" ++ body
