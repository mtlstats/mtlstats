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
editPlayerMenu = Menu "*** EDIT PLAYER ***" () $ map
  (\(ch, label, action) -> MenuItem ch label $ modify action)

  --  key, label,                 value
  [ ( '1', "Edit number",         set EPNumber   )
  , ( '2', "Edit name",           set EPName     )
  , ( '3', "Edit position",       set EPPosition )
  , ( '4', "Toggle rookie flag",  toggle         )
  , ( '5', "Edit YTD stats",      set EPYtd      )
  , ( '6', "Edit lifetime stats", set EPLifetime )
  , ( 'R', "Return to Edit Menu", edit           )
  ]

  where
    set mode = progMode.editPlayerStateL.epsMode .~ mode
    toggle   = editSelectedPlayer $ pRookie %~ not

-- | The 'Player' YTD stats edit menu
editPlayerYtdMenu :: Menu ()
editPlayerYtdMenu = editMenu
  "*** EDIT PLAYER YEAR-TO-DATE ***"
  --  key, label,                        value
  [ ( '1', "Edit all YTD stats",         EPYtdGoals   True  )
  , ( '2', "Edit YTD goals",             EPYtdGoals   False )
  , ( '3', "Edit YTD assists",           EPYtdAssists False )
  , ( '4', "Edit YTD penalty mins",      EPYtdPMin          )
  , ( 'R', "Return to player edit menu", EPMenu             )
  ]

-- | The 'Player' lifetime stats edit menu
editPlayerLtMenu :: Menu ()
editPlayerLtMenu = editMenu
  "*** EDIT PLAYER LIFETIME ***"
  --  key, label,                        value
  [ ( '1', "Edit all lifetime stats",    EPLtGoals   True  )
  , ( '2', "Edit lifetime goals",        EPLtGoals   False )
  , ( '3', "Edit lifetime assits",       EPLtAssists False )
  , ( '4', "Edit lifetime penalty mins", EPLtPMin          )
  , ( 'R', "Return to edit player menu", EPMenu            )
  ]

editMenu :: String -> [(Char, String, EditPlayerMode)] -> Menu ()
editMenu title = Menu title () . map
  (\(key, label, val) -> MenuItem key label $
    modify $ progMode.editPlayerStateL.epsMode .~ val)
