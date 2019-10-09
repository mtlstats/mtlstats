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
  strPrompt,
  numPrompt,
  -- * Individual prompts
  gameYearPrompt,
  gameDayPrompt,
  otherTeamPrompt,
  homeScorePrompt,
  awayScorePrompt,
  playerNumPrompt,
  playerNamePrompt,
  playerPosPrompt,
  selectPlayerPrompt,
  recordGoalPrompt,
  recordAssistPrompt,
  pMinPlayerPrompt
) where

import Control.Monad (when)
import Control.Monad.Trans.State (gets, modify)
import Data.Char (isDigit, toUpper)
import Data.Foldable (forM_)
import Lens.Micro ((^.), (&), (.~), (?~), (%~))
import Lens.Micro.Extras (view)
import Text.Read (readMaybe)
import qualified UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Config
import Mtlstats.Format
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
promptHandler p (C.EventCharacter c) = let
  c' = toUpper c
  in when (promptCharCheck p c') $
   modify $ addChar c'
promptHandler _ (C.EventSpecialKey C.KeyBackspace) =
  modify removeChar
promptHandler p (C.EventSpecialKey k) =
  promptSpecialKey p k
promptHandler _ _ = return ()

-- | Builds a string prompt
strPrompt
  :: String
  -- ^ The prompt string
  -> (String -> Action ())
  -- ^ The callback function for the result
  -> Prompt
strPrompt pStr act = Prompt
  { promptDrawer     = drawSimplePrompt pStr
  , promptCharCheck  = const True
  , promptAction     = act
  , promptSpecialKey = const $ return ()
  }

-- | Builds a numeric prompt
numPrompt
  :: String
  -- ^ The prompt string
  -> (Int -> Action ())
  -- ^ The callback function for the result
  -> Prompt
numPrompt pStr act = Prompt
  { promptDrawer     = drawSimplePrompt pStr
  , promptCharCheck  = isDigit
  , promptAction     = \inStr -> forM_ (readMaybe inStr) act
  , promptSpecialKey = const $ return ()
  }

-- | Prompts for the game year
gameYearPrompt :: Prompt
gameYearPrompt = numPrompt "Game year: " $
  modify . (progMode.gameStateL.gameYear ?~)

-- | Prompts for the day of the month the game took place
gameDayPrompt :: Prompt
gameDayPrompt = numPrompt "Day of month: " $
  modify . (progMode.gameStateL.gameDay ?~)

-- | Prompts for the other team name
otherTeamPrompt :: Prompt
otherTeamPrompt = strPrompt "Other team: " $
  modify . (progMode.gameStateL.otherTeam .~)

-- | Prompts for the home score
homeScorePrompt :: Prompt
homeScorePrompt = numPrompt "Home score: " $
  modify . (progMode.gameStateL.homeScore ?~)

-- | Prompts for the away score
awayScorePrompt :: Prompt
awayScorePrompt = numPrompt "Away score: " $
  modify . (progMode.gameStateL.awayScore ?~)

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
playerPosPrompt = strPrompt "Player position: " $
  modify . (progMode.createPlayerStateL.cpsPosition .~)

-- | Selects a player (creating one if necessary)
selectPlayerPrompt
  :: String
  -- ^ The prompt string
  -> (Maybe Int -> Action ())
  -- ^ The callback to run (takes the index number of the payer as
  -- input)
  -> Prompt
selectPlayerPrompt pStr callback = Prompt
  { promptDrawer = \s -> do
    let sStr = s^.inputBuffer
    C.drawString pStr
    C.drawString sStr
    (row, col) <- C.cursorPosition
    C.drawString "\n\nPlayer select:\n"
    let sel = zip [1..maxFunKeys] $ playerSearch sStr $ s^.database.dbPlayers
    mapM_
      (\(n, (_, p)) -> C.drawString $
        "F" ++ show n ++ ") " ++ p^.pName ++ " (" ++ show (p^.pNumber) ++ ")\n")
      sel
    C.moveCursor row col
  , promptCharCheck = const True
  , promptAction = \sStr -> if null sStr
    then callback Nothing
    else do
      players <- gets $ view $ database.dbPlayers
      case playerSearchExact sStr players of
        Just (n, _) -> callback $ Just n
        Nothing -> do
          mode <- gets $ view progMode
          let
            cps
              = newCreatePlayerState
              & cpsName .~ sStr
              & cpsSuccessCallback .~ do
                modify $ progMode .~ mode
                pIndex <- pred . length <$> gets (view $ database.dbPlayers)
                callback $ Just pIndex
              & cpsFailureCallback .~ do
                modify $ progMode .~ mode
                callback Nothing
          modify $ progMode .~ CreatePlayer cps
  , promptSpecialKey = \case
    C.KeyFunction n -> do
      sStr    <- gets $ view inputBuffer
      players <- gets $ view $ database.dbPlayers
      modify $ inputBuffer .~ ""
      let
        fKey    = pred $ fromIntegral n
        options = playerSearch sStr players
        sel     = fst <$> nth fKey options
      callback sel
    _ -> return ()
  }

-- | Prompts for the player who scored the goal
recordGoalPrompt
  :: Int
  -- ^ The game number
  -> Int
  -- ^ The goal number
  -> Prompt
recordGoalPrompt game goal = selectPlayerPrompt
  (  "*** GAME " ++ padNum 2 game ++ " ***\n"
  ++ "Who scored goal number " ++ show goal ++ "? "
  ) $ modify . (progMode.gameStateL.goalBy .~)

-- | Prompts for a player who assisted the goal
recordAssistPrompt
  :: Int
  -- ^ The game number
  -> Int
  -- ^ The goal nuber
  -> Int
  -- ^ The assist number
  -> Prompt
recordAssistPrompt game goal assist = selectPlayerPrompt
  (  "*** GAME " ++ padNum 2 game ++ " ***\n"
  ++ "Goal: " ++ show goal ++ "\n"
  ++ "Assist #" ++ show assist ++ ": "
  ) $ \case
    Nothing -> modify $ progMode.gameStateL.confirmGoalDataFlag .~ True
    Just n  -> do
      modify $ progMode.gameStateL.assistsBy %~ (++[n])
      nAssists <- length <$> gets (view $ progMode.gameStateL.assistsBy)
      when (nAssists >= maxAssists) $
        modify $ progMode.gameStateL.confirmGoalDataFlag .~ True

pMinPlayerPrompt :: Prompt
pMinPlayerPrompt = undefined

drawSimplePrompt :: String -> ProgState -> C.Update ()
drawSimplePrompt pStr s = C.drawString $ pStr ++ s^.inputBuffer
