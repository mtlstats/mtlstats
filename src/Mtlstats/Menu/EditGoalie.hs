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
  [ ( 'A', "EDIT NUMBER",         set EGNumber   )
  , ( 'B', "EDIT NAME",           set EGName     )
  , ( 'C', "TOGGLE ROOKIE FLAG",  toggleRookie   )
  , ( 'D', "TOGGLE ACTIVE FLAG",  toggleActive   )
  , ( 'E', "EDIT YTD STATS",      set EGYtd      )
  , ( 'F', "EDIT LIFETIME STATS", set EGLifetime )
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
  [ ( 'A', "EDIT ALL YTD STATS",  EGYtdGames    True  )
  , ( 'B', "EDIT YTD GAMES",      EGYtdGames    False )
  , ( 'C', "EDIT YTD MINUTES",    EGYtdMins     False )
  , ( 'D', "EDIT YTD GOALS",      EGYtdGoals    False )
  , ( 'E', "EDIT YTD SHUTOUTS",   EGYtdShutouts False )
  , ( 'F', "EDIT YTD WINS",       EGYtdWins     False )
  , ( 'G', "EDIT YTD LOSSES",     EGYtdLosses   False )
  , ( 'H', "EDIT YTD TIES",       EGYtdTies           )
  , ( 'R', "RETURN TO EDIT MENU", EGMenu              )
  ]

-- | The 'Goalie' lifetime edit menu
editGoalieLtMenu :: Menu ()
editGoalieLtMenu = editMenu
  "EDIT GOALTENDER LIFETIME"
  --  key, label,                   value
  [ ( 'A', "EDIT ALL LIFETIME STATS", EGLtGames    True  )
  , ( 'B', "EDIT LIFETIME GAMES",     EGLtGames    False )
  , ( 'C', "EDIT LIFETIME MINUTES",   EGLtMins     False )
  , ( 'D', "EDIT LIFETIME GOALS",     EGLtGoals    False )
  , ( 'E', "EDIT LIFETIME SHUTOUTS",  EGLtShutouts False )
  , ( 'F', "EDIT LIFETIME WINS",      EGLtWins     False )
  , ( 'G', "EDIT LIFETIME LOSSES",    EGLtLosses   False )
  , ( 'H', "EDIT LIFETIME TIES",      EGLtTies           )
  , ( 'R', "RETURN TO EDIT MENU",     EGMenu             )
  ]

editMenu :: String -> [(Char, String, EditGoalieMode)] -> Menu ()
editMenu title = Menu title () . map
  (\(key, label, val) -> MenuItem key label $
    modify $ progMode.editGoalieStateL.egsMode .~ val)
