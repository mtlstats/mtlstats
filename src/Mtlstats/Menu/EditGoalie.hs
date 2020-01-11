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
editGoalieMenu = Menu "*** EDIT GOALTENDER ***" () $ map
  (\(ch, label, action) -> MenuItem ch label $ modify action)

  --  key, label,                 value
  [ ( '1', "Edit number",         set EGNumber   )
  , ( '2', "Edit name",           set EGName     )
  , ( '3', "Toggle rookie flag",  toggleRookie   )
  , ( '4', "Toggle active flag",  toggleActive   )
  , ( '5', "Edit YTD stats",      set EGYtd      )
  , ( '6', "Edit Lifetime stats", set EGLifetime )
  , ( 'R', "Return to Edit Menu", edit           )
  ]

  where
    set mode     = progMode.editGoalieStateL.egsMode .~ mode
    toggleRookie = editSelectedGoalie (gRookie %~ not)
    toggleActive = editSelectedGoalie (gActive %~ not)

-- | The 'Goalie' YTD edit menu
editGoalieYtdMenu :: Menu ()
editGoalieYtdMenu = editMenu "*** EDIT GOALTENDER YEAR-TO-DATE ***"
  --  key, label,                 value
  [ ( '1', "Edit all YTD stats",  EGYtdGames    True  )
  , ( '2', "Edit YTD games",      EGYtdGames    False )
  , ( '3', "Edit YTD minutes",    EGYtdMins     False )
  , ( '4', "Edit YTD goals",      EGYtdGoals    False )
  , ( '5', "Edit YTD shutouts",   EGYtdShutouts False )
  , ( '6', "Edit YTD wins",       EGYtdWins     False )
  , ( '7', "Edit YTD losses",     EGYtdLosses   False )
  , ( '8', "Edit YTD ties",       EGYtdTies           )
  , ( 'R', "Return to edit menu", EGMenu              )
  ]

-- | The 'Goalie' lifetime edit menu
editGoalieLtMenu :: Menu ()
editGoalieLtMenu = editMenu
  "*** EDIT GOALTENDER LIFETIME ***"
  --  key, label,                   value
  [ ( '1', "Edit all lifetime stats", EGLtGames    True  )
  , ( '2', "Edit lifetime games",     EGLtGames    False )
  , ( '3', "Edit lifetime minutes",   EGLtMins     False )
  , ( '4', "Edit lifetime goals",     EGLtGoals    False )
  , ( '5', "Edit lifetime shutouts",  EGLtShutouts False )
  , ( '6', "Edit lifetime wins",      EGLtWins     False )
  , ( '7', "Edit lifetime losses",    EGLtLosses   False )
  , ( '8', "Edit lifetime ties",      EGLtTies           )
  , ( 'R', "Return to edit menu",     EGMenu             )
  ]

editMenu :: String -> [(Char, String, EditGoalieMode)] -> Menu ()
editMenu title = Menu title () . map
  (\(key, label, val) -> MenuItem key label $
    modify $ progMode.editGoalieStateL.egsMode .~ val)
