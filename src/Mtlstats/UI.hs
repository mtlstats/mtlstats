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

module Mtlstats.UI (draw) where

import Control.Monad (void)
import Lens.Micro ((^.))
import qualified UI.NCurses as C

import Mtlstats.Menu
import Mtlstats.Prompt
import Mtlstats.Types

-- | Drawing function
draw :: ProgState -> C.Curses ()
draw s = do
  void $ C.setCursorMode C.CursorInvisible
  w <- C.defaultWindow
  cm <- C.updateWindow w $ do
    C.clear
    case s ^. progMode of
      MainMenu  -> drawMenu mainMenu
      NewSeason -> drawMenu newSeasonMenu
      NewGame gs
        | null $ gs ^. gameType  -> drawMenu gameTypeMenu
        | null $ gs ^. homeScore -> drawPrompt homeScorePrompt s
        | otherwise              -> undefined
  C.render
  void $ C.setCursorMode cm
