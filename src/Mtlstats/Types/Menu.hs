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

{-# LANGUAGE TemplateHaskell #-}

module Mtlstats.Types.Menu (
  -- * Types
  Menu (..),
  MenuItem (..),
  -- * Lenses
  -- ** Menu Lenses
  menuTitle,
  menuDefault,
  menuItems,
  -- ** MenuItem Lenses
  miKey,
  miDescription,
  miAction,
) where

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)

import Mtlstats.Types

-- | Defines a menu
data Menu a = Menu
  { _menuTitle   :: String
  -- ^ The menu title
  , _menuDefault :: a
  -- ^ The value to return on incorrect selection or other event
  , _menuItems   :: [MenuItem a]
  -- ^ The list of items in the menu
  }

-- | Defines a menu item
data MenuItem a = MenuItem
  { _miKey         :: Char
  -- ^ The key that selects the menu item
  , _miDescription :: String
  -- ^ The description of the menu item
  , _miAction      :: Action a
  -- ^ The action to be performed when selected
  }

makeLenses ''Menu
makeLenses ''MenuItem

instance Show (Menu a) where
  show m = m ^. menuTitle ++ "\n" ++ items
    where items = unlines $ map show $ m ^. menuItems

instance Show (MenuItem a) where
  show i = [i ^. miKey] ++ ") " ++ i ^. miDescription
