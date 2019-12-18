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
import Lens.Micro ((.~))

import Mtlstats.Actions
import Mtlstats.Types
import Mtlstats.Types.Menu

-- | The 'Player' edit menu
editPlayerMenu :: Menu ()
editPlayerMenu = Menu "*** EDIT PLAYER ***" () $ map
  (\(ch, label, mode) -> MenuItem ch label $
    modify $ case mode of
      Nothing -> edit
      Just m  -> progMode.editPlayerStateL.epsMode .~ m)
  --  key, label,                 value
  [ ( '1', "Edit number",         Just EPNumber   )
  , ( '2', "Edit name",           Just EPName     )
  , ( '3', "Edit position",       Just EPPosition )
  , ( '4', "Edit YTD stats",      Just EPYtd      )
  , ( '5', "Edit lifetime stats", Just EPLifetime )
  , ( 'R', "Return to Edit Menu", Nothing         )
  ]

-- | The 'Player' YTD stats edit menu
editPlayerYtdMenu :: Menu ()
editPlayerYtdMenu = editMenu
  "*** EDIT PLAYER YEAR-TO-DATE ***"
  --  key, label,                        value
  [ ( '1', "Edit YTD goals",             EPYtdGoals   )
  , ( '2', "Edit YTD assists",           EPYtdAssists )
  , ( '3', "Edit YTD penalty mins",      EPYtdPMin    )
  , ( 'R', "Return to player edit menu", EPMenu       )
  ]

-- | The 'Player' lifetime stats edit menu
editPlayerLtMenu :: Menu ()
editPlayerLtMenu = editMenu
  "*** EDIT PLAYER LIFETIME ***"
  --  key, label,                        value
  [ ( '1', "Edit lifetime goals",        EPLtGoals   )
  , ( '2', "Edit lifetime assits",       EPLtAssists )
  , ( '3', "Edit lifetime penalty mins", EPLtPMin    )
  , ( 'R', "Return to edit player menu", EPMenu      )
  ]

editMenu :: String -> [(Char, String, EditPlayerMode)] -> Menu ()
editMenu title = Menu title () . map
  (\(key, label, val) -> MenuItem key label $
    modify $ progMode.editPlayerStateL.epsMode .~ val)
