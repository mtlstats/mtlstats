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
  ) where

import Control.Monad.Trans.State (modify)
import Lens.Micro ((.~))

import Mtlstats.Types
import Mtlstats.Types.Menu

-- | The player edit menu
editPlayerMenu :: Menu ()
editPlayerMenu = Menu "*** EDIT PLAYER ***" () $ map
  (\(ch, label, mode) -> MenuItem ch label $ case mode of
    Nothing -> modify $ progMode .~ MainMenu
    Just m  -> modify $ progMode.editPlayerStateL.epsMode .~ m)
  [ ( '1', "Change number",         Just EPNumber     )
  , ( '2', "Change name",           Just EPName       )
  , ( '3', "Change position",       Just EPPosition   )
  , ( '4', "YTD goals",             Just EPYtdGoals   )
  , ( '5', "YTD assists",           Just EPYtdAssists )
  , ( '6', "YTD penalty mins",      Just EPYtdPMin    )
  , ( '7', "Lifetime goals",        Just EPLtGoals    )
  , ( '8', "Lifetime assists",      Just EPLtAssists  )
  , ( '9', "Lifetime penalty mins", Just EPLtPMin     )
  , ( '0', "Finished editing",      Nothing           )
  ]
