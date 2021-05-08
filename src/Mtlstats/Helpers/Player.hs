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

module Mtlstats.Helpers.Player (playerDetails, playerName) where

import Lens.Micro ((^.))

import Mtlstats.Format
import Mtlstats.Types

-- | Provides a detailed string describing a 'Player'
playerDetails :: Player -> String
playerDetails p = unlines $ top ++ [""] ++ table
  where
    top = labelTable
      [ ( "Number",   show $ p^.pNumber )
      , ( "Name",     playerName p      )
      , ( "Position", p^.pPosition      )
      ]

    table = numTable ["YTD", "Lifetime"] $ map
      (\(label, lens) ->
        (label, [p^.pYtd.lens, p^.pLifetime.lens]))
      [ ( "Goals",        psGoals   )
      , ( "Assists",      psAssists )
      , ( "Penalty mins", psPMin    )
      ]

-- | Presents a modified version of the player's name indicating
-- whether or not they're a rookie
playerName :: Player -> String
playerName p = let

  prefix = if p^.pActive
    then ""
    else "*"

  suffix = if p^.pRookie
    then "*"
    else ""

  in prefix ++ p^.pName ++ suffix
