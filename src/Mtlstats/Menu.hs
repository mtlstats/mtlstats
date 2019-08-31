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
  -- * Menu Functions
  drawMenu,
  menuHandler,
  -- * Menus
  mainMenu,
  newSeasonMenu,
  gameTypeMenu
) where

import Control.Monad.Trans.State (modify)
import Lens.Micro ((^.), (.~), (?~))
import qualified UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Types
import Mtlstats.Types.Menu

-- | The draw function for a 'Menu'
drawMenu :: Menu a -> C.Update C.CursorMode
drawMenu m = do
  C.drawString $ show m
  return C.CursorInvisible

-- | The event handler for a 'Menu'
menuHandler :: Menu a -> C.Event -> Action a
menuHandler m (C.EventCharacter c) =
  case filter (\i -> i ^. miKey == c) $ m ^. menuItems of
    i:_ -> i ^. miAction
    []  -> return $ m ^. menuDefault
menuHandler m _ = return $ m ^. menuDefault

-- | The main menu
mainMenu :: Menu Bool
mainMenu = Menu "*** MAIN MENU ***" True
  [ MenuItem '1' "New Season" $
    modify startNewSeason >> return True
  , MenuItem '2' "New Game" $
    modify startNewGame >> return True
  , MenuItem '3' "Exit" $
    return False
  ]

-- | The new season menu
newSeasonMenu :: Menu ()
newSeasonMenu = Menu "*** SEASON TYPE ***" ()
  [ MenuItem '1' "Regular Season" $
    modify $ resetYtd . startNewGame
  , MenuItem '2' "Playoffs" $
    modify startNewGame
  ]

-- | The game type menu (home/away)
gameTypeMenu :: Menu ()
gameTypeMenu = Menu "*** GAME TYPE ***" ()
  [ MenuItem '1' "Home Game" $
    modify $ progMode.gameStateL.gameType ?~ HomeGame
  , MenuItem '2' "Away Game" $
    modify $ progMode.gameStateL.gameType ?~ AwayGame
  ]
