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
  numPrompt,
  selectPrompt,
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
  selectGoaliePrompt,
  recordGoalPrompt,
  recordAssistPrompt,
  pMinPlayerPrompt,
  assignPMinsPrompt,
  goalieNumPrompt,
  goalieNamePrompt,
  selectGameGoaliePrompt,
  goalieMinsPlayedPrompt,
  goalsAllowedPrompt,
  playerToEditPrompt
) where

import Control.Monad (when)
import Control.Monad.Extra (whenJust)
import Control.Monad.Trans.State (gets, modify)
import Data.Char (isDigit, toUpper)
import Data.Foldable (forM_)
import Data.Maybe (fromMaybe)
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
  , promptCharCheck = const True
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
        whenJust (nth n results) $ \(n, _) -> do
          modify $ inputBuffer .~ ""
          spCallback params $ Just n
    _ -> return ()
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
selectPlayerPrompt pStr callback = selectPrompt SelectParams
  { spPrompt       = pStr
  , spSearchHeader = "Player select:"
  , spSearch       = \sStr db -> playerSearch sStr (db^.dbPlayers)
  , spSearchExact  = \sStr db -> fst <$> playerSearchExact sStr (db^.dbPlayers)
  , spElemDesc     = playerSummary
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

-- | Prompts for the player to assign penalty minutes to
pMinPlayerPrompt :: Prompt
pMinPlayerPrompt = selectPlayerPrompt
  "Assign penalty minutes to: " $
  \case
    Nothing -> modify $ progMode.gameStateL.pMinsRecorded .~ True
    Just n  -> modify $ progMode.gameStateL.selectedPlayer ?~ n

-- | Prompts for the number of penalty mintues to assign to the player
assignPMinsPrompt :: Prompt
assignPMinsPrompt = numPrompt "Penalty minutes: " $
  modify . assignPMins

-- | Prompts tor the goalie's number
goalieNumPrompt :: Prompt
goalieNumPrompt = numPrompt "Goalie number: " $
  modify . (progMode.createGoalieStateL.cgsNumber ?~)

-- | Prompts for the goalie's name
goalieNamePrompt :: Prompt
goalieNamePrompt = strPrompt "Goalie name: " $
  modify . (progMode.createGoalieStateL.cgsName .~)

-- | Prompts for a goalie who played in the game
selectGameGoaliePrompt :: Prompt
selectGameGoaliePrompt = selectGoaliePrompt "Which goalie played this game: " $
  \case
    Nothing -> modify $ progMode.gameStateL.goaliesRecorded .~ True
    Just n  -> modify $ progMode.gameStateL.gameSelectedGoalie  ?~ n

-- | Prompts for the number of minutes the goalie has played
goalieMinsPlayedPrompt :: Prompt
goalieMinsPlayedPrompt = numPrompt "Minutes played: " $
  modify . (progMode.gameStateL.goalieMinsPlayed ?~)

-- | Prompts for the number of goals the goalie allowed
goalsAllowedPrompt :: Prompt
goalsAllowedPrompt = numPrompt "Goals allowed: " $ \n -> do
  modify (progMode.gameStateL.goalsAllowed ?~ n)
  mins <- fromMaybe 0 <$> gets (^.progMode.gameStateL.goalieMinsPlayed)
  when (mins >= gameLength) $
    modify $ progMode.gameStateL.goaliesRecorded .~ True
  modify recordGoalieStats

playerToEditPrompt :: Prompt
playerToEditPrompt = selectPlayerPrompt "Player to edit: " $
  modify . (progMode.editPlayerStateL.epsSelectedPlayer .~)

drawSimplePrompt :: String -> ProgState -> C.Update ()
drawSimplePrompt pStr s = C.drawString $ pStr ++ s^.inputBuffer
