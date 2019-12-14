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

{-# LANGUAGE LambdaCase #-}

module Mtlstats.Prompt (
  -- * Prompt Functions
  drawPrompt,
  promptHandler,
  promptControllerWith,
  promptController,
  strPrompt,
  ucStrPrompt,
  numPrompt,
  selectPrompt,
  -- * Individual prompts
  playerNumPrompt,
  playerNamePrompt,
  playerPosPrompt,
  goalieNumPrompt,
  goalieNamePrompt,
  selectPlayerPrompt,
  selectGoaliePrompt,
  playerToEditPrompt
) where

import Control.Monad (when)
import Control.Monad.Extra (whenJust)
import Control.Monad.Trans.State (gets, modify)
import Data.Char (isDigit, toUpper)
import Data.Foldable (forM_)
import Lens.Micro ((^.), (&), (.~), (?~), (%~))
import Lens.Micro.Extras (view)
import Text.Read (readMaybe)
import qualified UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Config
import Mtlstats.Types
import Mtlstats.Util

-- | Draws the prompt to the screen
drawPrompt :: Prompt -> ProgState -> C.Update C.CursorMode
drawPrompt p s = do
  promptDrawer p s
  return C.CursorVisible

-- | Event handler for a prompt
promptHandler :: Prompt -> C.Event -> Action ()
promptHandler p (C.EventCharacter '\n') = do
  val <- gets $ view inputBuffer
  modify $ inputBuffer .~ ""
  promptAction p val
promptHandler p (C.EventCharacter c) =
  modify $ inputBuffer %~ promptProcessChar p c
promptHandler _ (C.EventSpecialKey C.KeyBackspace) =
  modify removeChar
promptHandler p (C.EventSpecialKey k) =
  promptSpecialKey p k
promptHandler _ _ = return ()

-- | Builds a controller out of a prompt with a header
promptControllerWith
  :: (ProgState -> C.Update ())
  -- ^ The header
  -> Prompt
  -- ^ The prompt to use
  -> Controller
  -- ^ The resulting controller
promptControllerWith header prompt = Controller
  { drawController = \s -> do
    header s
    drawPrompt prompt s
  , handleController = \e -> do
    promptHandler prompt e
    return True
  }

-- | Builds a controller out of a prompt
promptController
  :: Prompt
  -- ^ The prompt to use
  -> Controller
  -- ^ The resulting controller
promptController = promptControllerWith (const $ return ())

-- | Builds a string prompt
strPrompt
  :: String
  -- ^ The prompt string
  -> (String -> Action ())
  -- ^ The callback function for the result
  -> Prompt
strPrompt pStr act = Prompt
  { promptDrawer      = drawSimplePrompt pStr
  , promptProcessChar = \ch -> (++ [ch])
  , promptAction      = act
  , promptSpecialKey  = const $ return ()
  }

-- | Creates an upper case string prompt
ucStrPrompt
  :: String
  -- ^ The prompt string
  -> (String -> Action ())
  -- ^ The callback function for the result
  -> Prompt
ucStrPrompt pStr act = (strPrompt pStr act)
  { promptProcessChar = \ch -> (++ [toUpper ch]) }

-- | Builds a numeric prompt
numPrompt
  :: String
  -- ^ The prompt string
  -> (Int -> Action ())
  -- ^ The callback function for the result
  -> Prompt
numPrompt pStr act = Prompt
  { promptDrawer      = drawSimplePrompt pStr
  , promptProcessChar = \ch str -> if isDigit ch
    then str ++ [ch]
    else str
  , promptAction      = \inStr -> forM_ (readMaybe inStr) act
  , promptSpecialKey  = const $ return ()
  }

-- | Builds a selection prompt
selectPrompt :: SelectParams a -> Prompt
selectPrompt params = Prompt
  { promptDrawer = \s -> do
    let sStr = s^.inputBuffer
    C.drawString $ spPrompt params ++ sStr
    (row, col) <- C.cursorPosition
    C.drawString $ "\n\n" ++ spSearchHeader params ++ "\n"
    let results = zip [1..maxFunKeys] $ spSearch params sStr (s^.database)
    C.drawString $ unlines $ map
      (\(n, (_, x)) -> let
        desc = spElemDesc params x
        in "F" ++ show n ++ ") " ++ desc)
      results
    C.moveCursor row col
  , promptProcessChar = spProcessChar params
  , promptAction = \sStr -> if null sStr
    then spCallback params Nothing
    else do
      db <- gets (^.database)
      case spSearchExact params sStr db of
        Nothing -> spNotFound params sStr
        Just n  -> spCallback params $ Just n
  , promptSpecialKey = \case
    C.KeyFunction rawK -> do
      sStr <- gets (^.inputBuffer)
      db   <- gets (^.database)
      let
        n       = pred $ fromInteger rawK
        results = spSearch params sStr db
      when (n < maxFunKeys) $
        whenJust (nth n results) $ \(sel, _) -> do
          modify $ inputBuffer .~ ""
          spCallback params $ Just sel
    _ -> return ()
  }

-- | Prompts for a new player's number
playerNumPrompt :: Prompt
playerNumPrompt = numPrompt "Player number: " $
  modify . (progMode.createPlayerStateL.cpsNumber ?~)

-- | Prompts for a new player's name
playerNamePrompt :: Prompt
playerNamePrompt = strPrompt "Player name: " $
  modify . (progMode.createPlayerStateL.cpsName .~)

-- | Prompts for a new player's position
playerPosPrompt :: Prompt
playerPosPrompt = ucStrPrompt "Player position: " $
  modify . (progMode.createPlayerStateL.cpsPosition .~)

-- | Prompts tor the goalie's number
goalieNumPrompt :: Prompt
goalieNumPrompt = numPrompt "Goalie number: " $
  modify . (progMode.createGoalieStateL.cgsNumber ?~)

-- | Prompts for the goalie's name
goalieNamePrompt :: Prompt
goalieNamePrompt = strPrompt "Goalie name: " $
  modify . (progMode.createGoalieStateL.cgsName .~)

-- | Selects a player (creating one if necessary)
selectPlayerPrompt
  :: String
  -- ^ The prompt string
  -> (Maybe Int -> Action ())
  -- ^ The callback to run (takes the index number of the payer as
  -- input)
  -> Prompt
selectPlayerPrompt pStr callback = selectPrompt SelectParams
  { spPrompt       = pStr
  , spSearchHeader = "Player select:"
  , spSearch       = \sStr db -> playerSearch sStr (db^.dbPlayers)
  , spSearchExact  = \sStr db -> fst <$> playerSearchExact sStr (db^.dbPlayers)
  , spElemDesc     = playerSummary
  , spProcessChar  = capitalizeName
  , spCallback     = callback
  , spNotFound     = \sStr -> do
    mode <- gets (^.progMode)
    let
      cps = newCreatePlayerState
        & cpsName .~ sStr
        & cpsSuccessCallback .~ do
          modify $ progMode .~ mode
          index <- pred . length <$> gets (^.database.dbPlayers)
          callback $ Just index
        & cpsFailureCallback .~ modify (progMode .~ mode)
    modify $ progMode .~ CreatePlayer cps
  }

-- | Selects a goalie (creating one if necessary)
selectGoaliePrompt
  :: String
  -- ^ The prompt string
  -> (Maybe Int -> Action ())
  -- ^ The callback to run (takes the index number of the goalie as
  -- input)
  -> Prompt
selectGoaliePrompt pStr callback = selectPrompt SelectParams
  { spPrompt       = pStr
  , spSearchHeader = "Goalie select:"
  , spSearch       = \sStr db -> goalieSearch sStr (db^.dbGoalies)
  , spSearchExact  = \sStr db -> fst <$> goalieSearchExact sStr (db^.dbGoalies)
  , spElemDesc     = goalieSummary
  , spProcessChar  = capitalizeName
  , spCallback     = callback
  , spNotFound     = \sStr -> do
    mode <- gets (^.progMode)
    let
      cgs = newCreateGoalieState
        & cgsName .~ sStr
        & cgsSuccessCallback .~ do
          modify $ progMode .~ mode
          index <- pred . length <$> gets (^.database.dbGoalies)
          callback $ Just index
        & cgsFailureCallback .~ modify (progMode .~ mode)
    modify $ progMode .~ CreateGoalie cgs
  }

playerToEditPrompt :: Prompt
playerToEditPrompt = selectPlayerPrompt "Player to edit: " $
  modify . (progMode.editPlayerStateL.epsSelectedPlayer .~)

drawSimplePrompt :: String -> ProgState -> C.Update ()
drawSimplePrompt pStr s = C.drawString $ pStr ++ s^.inputBuffer
