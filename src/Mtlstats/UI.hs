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

import Lens.Micro ((^.))
import qualified UI.NCurses as C

import Mtlstats.Menu
import Mtlstats.Types

-- | Drawing function
draw :: ProgState -> C.Curses ()
draw s = do
  w <- C.defaultWindow
  C.updateWindow w $ do
    C.clear
    case s ^. progMode of
      MainMenu  -> drawMenu mainMenu
      NewSeason -> newSeason
      NewGame   -> newGame
  C.render

newSeason :: C.Update ()
newSeason = C.drawString $ unlines
  [ "*** SEASON TYPE ***"
  , "1) Regular Season"
  , "2) Playoffs"
  ]

newGame :: C.Update ()
newGame = return ()
