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

module Mtlstats.Menu (
  drawMenu,
  menuHandler,
) where

import Control.Monad.Trans.State (StateT)
import Lens.Micro ((^.))
import qualified UI.NCurses as C

import Mtlstats.Types
import Mtlstats.Types.Menu

-- | The draw function for a 'Menu'
drawMenu :: Menu a -> C.Update ()
drawMenu = C.drawString . show

-- | The event handler for a 'Menu'
menuHandler :: Menu a -> C.Event -> StateT ProgState C.Curses a
menuHandler m (C.EventCharacter c) =
  case filter (\i -> i ^. miKey == c) $ m ^. menuItems of
    i:_ -> i ^. miAction
    []  -> return $ m ^. menuDefault
menuHandler m _ = return $ m ^. menuDefault
