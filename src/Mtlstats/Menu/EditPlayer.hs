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

module Mtlstats.Menu.EditPlayer
  ( editPlayerMenu
  , editPlayerYtdMenu
  , editPlayerLtMenu
  ) where

import Control.Monad.Trans.State (modify)
import Lens.Micro ((.~), (%~))

import Mtlstats.Actions
import Mtlstats.Types
import Mtlstats.Types.Menu

-- | The 'Player' edit menu
editPlayerMenu :: Menu ()
editPlayerMenu = Menu "EDIT PLAYER" () $ map
  (\(ch, label, action) -> MenuItem ch label $ modify action)

  --  key, label,                 value
  [ ( 'A', "NUMBER",              set EPNumber   )
  , ( 'B', "NAME",                set EPName     )
  , ( 'C', "POSITION",            set EPPosition )
  , ( 'D', "ROOKIE FLAG",         toggleRookie   )
  , ( 'E', "ACTIVE FLAG",         toggleActive   )
  , ( 'F', "YTD STATS",           set EPYtd      )
  , ( 'G', "LIFETIME STATS",      set EPLifetime )
  , ( 'H', "DELETE RECORD",       set EPDelete   )
  , ( 'R', "RETURN TO EDIT MENU", edit           )
  ]

  where
    set mode     = progMode.editPlayerStateL.epsMode .~ mode
    toggleRookie = editSelectedPlayer $ pRookie %~ not
    toggleActive = editSelectedPlayer $ pActive %~ not

-- | The 'Player' YTD stats edit menu
editPlayerYtdMenu :: Menu ()
editPlayerYtdMenu = editMenu
  "EDIT PLAYER YEAR-TO-DATE"
  --  key, label,                        value
  [ ( 'A', "ALL YTD STATS",              EPYtdGoals   True  )
  , ( 'B', "YTD GOALS",                  EPYtdGoals   False )
  , ( 'C', "YTD ASSISTS",                EPYtdAssists False )
  , ( 'D', "YTD PENALTY MINS",           EPYtdPMin          )
  , ( 'R', "RETURN TO PLAYER EDIT MENU", EPMenu             )
  ]

-- | The 'Player' lifetime stats edit menu
editPlayerLtMenu :: Menu ()
editPlayerLtMenu = editMenu
  "EDIT PLAYER LIFETIME"
  --  key, label,                        value
  [ ( 'A', "ALL LIFETIME STATS",         EPLtGoals   True  )
  , ( 'B', "LIFETIME GOALS",             EPLtGoals   False )
  , ( 'C', "LIFETIME ASSITS",            EPLtAssists False )
  , ( 'D', "LIFETIME PENALTY MINS",      EPLtPMin          )
  , ( 'R', "RETURN TO EDIT PLAYER MENU", EPMenu            )
  ]

editMenu :: String -> [(Char, String, EditPlayerMode)] -> Menu ()
editMenu title = Menu title () . map
  (\(key, label, val) -> MenuItem key label $
    modify $ progMode.editPlayerStateL.epsMode .~ val)
