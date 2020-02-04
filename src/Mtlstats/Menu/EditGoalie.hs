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

module Mtlstats.Menu.EditGoalie
  ( editGoalieMenu
  , editGoalieYtdMenu
  , editGoalieLtMenu
  ) where

import Control.Monad.Trans.State (modify)
import Lens.Micro ((.~), (%~))

import Mtlstats.Actions
import Mtlstats.Types
import Mtlstats.Types.Menu

-- | The 'Goalie' edit menu
editGoalieMenu :: Menu ()
editGoalieMenu = Menu "EDIT GOALTENDER" () $ map
  (\(ch, label, action) -> MenuItem ch label $ modify action)

  --  key, label,                 value
  [ ( 'A', "NUMBER",              set EGNumber   )
  , ( 'B', "NAME",                set EGName     )
  , ( 'C', "ROOKIE FLAG",         toggleRookie   )
  , ( 'D', "ACTIVE FLAG",         toggleActive   )
  , ( 'E', "YTD STATS",           set EGYtd      )
  , ( 'F', "LIFETIME STATS",      set EGLifetime )
  , ( 'R', "RETURN TO EDIT MENU", edit           )
  ]

  where
    set mode     = progMode.editGoalieStateL.egsMode .~ mode
    toggleRookie = editSelectedGoalie (gRookie %~ not)
    toggleActive = editSelectedGoalie (gActive %~ not)

-- | The 'Goalie' YTD edit menu
editGoalieYtdMenu :: Menu ()
editGoalieYtdMenu = editMenu "EDIT GOALTENDER YEAR-TO-DATE"
  --  key, label,                 value
  [ ( 'A', "ALL YTD STATS",       EGYtdGames    True  )
  , ( 'B', "YTD GAMES",           EGYtdGames    False )
  , ( 'C', "YTD MINUTES",         EGYtdMins     False )
  , ( 'D', "YTD GOALS",           EGYtdGoals    False )
  , ( 'E', "YTD SHUTOUTS",        EGYtdShutouts False )
  , ( 'F', "YTD WINS",            EGYtdWins     False )
  , ( 'G', "YTD LOSSES",          EGYtdLosses   False )
  , ( 'H', "YTD TIES",            EGYtdTies           )
  , ( 'R', "RETURN TO EDIT MENU", EGMenu              )
  ]

-- | The 'Goalie' lifetime edit menu
editGoalieLtMenu :: Menu ()
editGoalieLtMenu = editMenu
  "EDIT GOALTENDER LIFETIME"
  --  key, label,                 value
  [ ( 'A', "ALL LIFETIME STATS",  EGLtGames    True  )
  , ( 'B', "LIFETIME GAMES",      EGLtGames    False )
  , ( 'C', "LIFETIME MINUTES",    EGLtMins     False )
  , ( 'D', "LIFETIME GOALS",      EGLtGoals    False )
  , ( 'E', "LIFETIME SHUTOUTS",   EGLtShutouts False )
  , ( 'F', "LIFETIME WINS",       EGLtWins     False )
  , ( 'G', "LIFETIME LOSSES",     EGLtLosses   False )
  , ( 'H', "LIFETIME TIES",       EGLtTies           )
  , ( 'R', "RETURN TO EDIT MENU", EGMenu             )
  ]

editMenu :: String -> [(Char, String, EditGoalieMode)] -> Menu ()
editMenu title = Menu title () . map
  (\(key, label, val) -> MenuItem key label $
    modify $ progMode.editGoalieStateL.egsMode .~ val)
