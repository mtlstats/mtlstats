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

module Mtlstats.Menu.EditGoalie
  ( editGoalieMenu
  , editGoalieYtdMenu
  ) where

import Control.Monad.Trans.State (modify)
import Data.Maybe (maybe)
import Lens.Micro ((.~))

import Mtlstats.Types
import Mtlstats.Types.Menu

-- | The 'Goalie' edit menu
editGoalieMenu :: Menu ()
editGoalieMenu = Menu "*** EDIT GOALTENDER ***" () $ map
  (\(key, label, val) -> MenuItem key label $ modify $ maybe
    (progMode .~ MainMenu)
    (progMode.editGoalieStateL.egsMode .~)
    val)
  --  key, label,                 value
  [ ( '1', "Edit number",         Just EGNumber   )
  , ( '2', "Edit name",           Just EGName     )
  , ( '3', "Edit YTD stats",      Just EGYtd      )
  , ( '4', "Edit Lifetime stats", Just EGLifetime )
  , ( 'R', "Return to Main Menu", Nothing         )
  ]

-- | The 'Goalie' YTD edit menu
editGoalieYtdMenu :: Menu ()
editGoalieYtdMenu = undefined
