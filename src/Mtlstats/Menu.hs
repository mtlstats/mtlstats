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

module Mtlstats.Menu (
  -- * Menu Functions
  menuController,
  menuControllerWith,
  menuStateController,
  drawMenu,
  menuHandler,
  -- * Menus
  mainMenu,
  newSeasonMenu,
  gameMonthMenu,
  gameTypeMenu,
  gameGoalieMenu,
  editMenu
) where

import Control.Monad.Trans.State (gets, modify)
import Data.Char (toUpper)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Lens.Micro ((^.), (?~))
import qualified UI.NCurses as C

import Mtlstats.Actions
import qualified Mtlstats.Actions.NewGame.GoalieInput as GI
import Mtlstats.Actions.EditStandings
import Mtlstats.Config
import Mtlstats.Format
import Mtlstats.Types
import Mtlstats.Types.Menu
import Mtlstats.Util

-- | Generates a simple 'Controller' for a Menu
menuController :: Menu () -> Controller
menuController = menuControllerWith $ const $ return ()

-- | Generate a simple 'Controller' for a 'Menu' with a header
menuControllerWith
  :: (ProgState -> C.Update ())
  -- ^ Generates the header
  -> Menu ()
  -- ^ The menu
  -> Controller
  -- ^ The resulting controller
menuControllerWith header menu = Controller
  { drawController = \s -> do
    header s
    drawMenu menu
  , handleController = \e -> do
    menuHandler menu e
    return True
  }

-- | Generate and create a controller for a menu based on the current
-- 'ProgState'
menuStateController
  :: (ProgState -> Menu ())
  -- ^ The function to generate the menu
  -> Controller
  -- ^ The resulting controller
menuStateController menuFunc = Controller
  { drawController = drawMenu . menuFunc
  , handleController = \e -> do
    menu <- gets menuFunc
    menuHandler menu e
    return True
  }

-- | The draw function for a 'Menu'
drawMenu :: Menu a -> C.Update C.CursorMode
drawMenu m = do
  (_, cols) <- C.windowSize
  let
    width    = fromIntegral $ pred cols
    menuText = map (centre width) $ lines $ show m
  C.drawString $ unlines menuText
  return C.CursorInvisible

-- | The event handler for a 'Menu'
menuHandler :: Menu a -> C.Event -> Action a
menuHandler m (C.EventCharacter c) =
  case filter (\i -> i^.miKey == toUpper c) $ m^.menuItems of
    i:_ -> i^.miAction
    []  -> return $ m^.menuDefault
menuHandler m _ = return $ m^.menuDefault

-- | The main menu
mainMenu :: Menu Bool
mainMenu = Menu "MASTER MENU" True
  [ MenuItem 'A' "NEW SEASON" $
    modify startNewSeason >> return True
  , MenuItem 'B' "NEW GAME" $
    modify startNewGame >> return True
  , MenuItem 'C' "EDIT MENU" $
    modify edit >> return True
  , MenuItem 'E' "EXIT" $
    saveDatabase dbFname >> return False
  ]

-- | The new season menu
newSeasonMenu :: Menu ()
newSeasonMenu = Menu "SEASON TYPE" ()
  [ MenuItem 'R' "REGULAR SEASON" $ modify
    $ resetYtd
    . clearRookies
    . resetStandings
    . startNewGame
  , MenuItem 'P' "PLAYOFFS" $ modify
    $ resetStandings
    . startNewGame
  ]

-- | Requests the month in which the game took place
gameMonthMenu :: Menu ()
gameMonthMenu = Menu "MONTH:" () $ map
  (\(ch, name, val) ->
    MenuItem ch name $
    modify $ progMode.gameStateL.gameMonth ?~ val)
  [ ( 'A', "JANUARY",   1  )
  , ( 'B', "FEBRUARY",  2  )
  , ( 'C', "MARCH",     3  )
  , ( 'D', "APRIL",     4  )
  , ( 'E', "MAY",       5  )
  , ( 'F', "JUNE",      6  )
  , ( 'G', "JULY",      7  )
  , ( 'H', "AUGUST",    8  )
  , ( 'I', "SEPTEMBER", 9  )
  , ( 'J', "OCTOBER",   10 )
  , ( 'K', "NOVEMBER",  11 )
  , ( 'L', "DECEMBER",  12 )
  ]

-- | The game type menu (home/away)
gameTypeMenu :: Menu ()
gameTypeMenu = Menu "GAME TYPE:" ()
  [ MenuItem 'H' "HOME GAME" $
    modify $ progMode.gameStateL.gameType ?~ HomeGame
  , MenuItem 'A' "AWAY GAME" $
    modify $ progMode.gameStateL.gameType ?~ AwayGame
  ]

-- | Game goalie selection menu
gameGoalieMenu :: ProgState -> Menu ()
gameGoalieMenu s = let
  title   = "Which goalie should get credit for the game?"
  gids    = map fst $ M.toList $ s^.progMode.gameStateL.gameGoalieStats
  goalies = mapMaybe
    (\n -> do
      goalie <- nth n $ s^.database.dbGoalies
      Just (n, goalie))
    gids
  in Menu title () $ zipWith
    (\ch (gid, goalie) -> MenuItem ch (goalieSummary goalie) $
      modify $ GI.setGameGoalie gid)
    ['1'..]
    goalies

-- | The edit menu
editMenu :: Menu ()
editMenu = Menu "EDIT MENU" ()
  [ MenuItem 'A' "CREATE PLAYER" $
    modify createPlayer
  , MenuItem 'B' "CREATE GOALIE" $
    modify createGoalie
  , MenuItem 'C' "EDIT PLAYER" $
    modify editPlayer
  , MenuItem 'D' "EDIT GOALIE" $
    modify editGoalie
  , MenuItem 'E' "EDIT STANDINGS" $
    modify editStandings
  , MenuItem 'R' "RETURN TO MAIN MENU" $
    modify backHome
  ]
