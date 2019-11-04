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
  menuController,
  drawMenu,
  menuHandler,
  -- * Menus
  mainMenu,
  newSeasonMenu,
  gameMonthMenu,
  gameTypeMenu,
  editPlayerMenu,
  gameGoalieMenu
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (gets, modify)
import Data.Aeson (encodeFile)
import Data.Char (toUpper)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Lens.Micro ((^.), (.~), (?~))
import Lens.Micro.Extras (view)
import System.EasyFile
  ( createDirectoryIfMissing
  , getAppUserDataDirectory
  , (</>)
  )
import qualified UI.NCurses as C

import Mtlstats.Actions
import qualified Mtlstats.Actions.GoalieInput as GI
import Mtlstats.Config
import Mtlstats.Types
import Mtlstats.Types.Menu
import Mtlstats.Util

-- | Generates a simple 'Controller' for a Menu
menuController :: Menu () -> Controller
menuController menu = Controller
  { drawController   = const $ drawMenu menu
  , handleController = \e -> do
    menuHandler menu e
    return True
  }

-- | The draw function for a 'Menu'
drawMenu :: Menu a -> C.Update C.CursorMode
drawMenu m = do
  C.drawString $ show m
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
mainMenu = Menu "*** MAIN MENU ***" True
  [ MenuItem '1' "New Season" $
    modify startNewSeason >> return True
  , MenuItem '2' "New Game" $
    modify startNewGame >> return True
  , MenuItem '3' "Create Player" $
    modify createPlayer >> return True
  , MenuItem '4' "Create Goalie" $
    modify createGoalie >> return True
  , MenuItem '5' "Edit Player" $
    modify editPlayer >> return True
  , MenuItem '6' "Exit" $ do
    db <- gets $ view database
    liftIO $ do
      dir <- getAppUserDataDirectory appName
      let dbFile = dir </> dbFname
      createDirectoryIfMissing True dir
      encodeFile dbFile db
    return False
  ]

-- | The new season menu
newSeasonMenu :: Menu ()
newSeasonMenu = Menu "*** SEASON TYPE ***" ()
  [ MenuItem '1' "Regular Season" $ do
    modify resetYtd
    modify startNewGame
  , MenuItem '2' "Playoffs" $
    modify startNewGame
  ]

-- | Requests the month in which the game took place
gameMonthMenu :: Menu ()
gameMonthMenu = Menu "Month:" () $ map
  (\(ch, name, val) ->
    MenuItem ch name $
    modify $ progMode.gameStateL.gameMonth ?~ val)
  [ ( 'A', "January",   1  )
  , ( 'B', "February",  2  )
  , ( 'C', "March",     3  )
  , ( 'D', "April",     4  )
  , ( 'E', "May",       5  )
  , ( 'F', "June",      6  )
  , ( 'G', "July",      7  )
  , ( 'H', "August",    8  )
  , ( 'I', "September", 9  )
  , ( 'J', "October",   10 )
  , ( 'K', "November",  11 )
  , ( 'L', "December",  12 )
  ]

-- | The game type menu (home/away)
gameTypeMenu :: Menu ()
gameTypeMenu = Menu "Game type:" ()
  [ MenuItem '1' "Home Game" $
    modify $ progMode.gameStateL.gameType ?~ HomeGame
  , MenuItem '2' "Away Game" $
    modify $ progMode.gameStateL.gameType ?~ AwayGame
  ]

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
  in Menu title () $ map
    (\(ch, (gid, goalie)) -> MenuItem ch (goalieSummary goalie) $
      modify $ GI.setGameGoalie gid) $
    zip ['1'..] goalies
