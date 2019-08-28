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

module Mtlstats.Prompt (
  -- * Prompt Functions
  drawPrompt,
  promptHandler,
  strPrompt,
  numPrompt,
  -- * Individual prompts
  otherTeamPrompt,
  homeScorePrompt,
  awayScorePrompt
) where

import Control.Monad (when)
import Control.Monad.Trans.State (gets, modify)
import Data.Char (isDigit, toUpper)
import Data.Foldable (forM_)
import Lens.Micro ((^.), (.~), (?~))
import Lens.Micro.Extras (view)
import Text.Read (readMaybe)
import qualified UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Types

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
promptHandler p (C.EventSpecialKey (C.KeyFunction k)) =
  promptFunctionKey p k
promptHandler _ _ = return ()

-- | Builds a string prompt
strPrompt
  :: String
  -- ^ The prompt string
  -> (String -> Action ())
  -- ^ The callback function for the result
  -> Prompt
strPrompt pStr act = Prompt
  { promptDrawer      = drawSimplePrompt pStr
  , promptCharCheck   = const True
  , promptAction      = act
  , promptFunctionKey = const $ return ()
  }

-- | Builds a numeric prompt
numPrompt
  :: String
  -- ^ The prompt string
  -> (Int -> Action ())
  -- ^ The callback function for the result
  -> Prompt
numPrompt pStr act = Prompt
  { promptDrawer      = drawSimplePrompt pStr
  , promptCharCheck   = isDigit
  , promptAction      = \inStr -> forM_ (readMaybe inStr) act
  , promptFunctionKey = const $ return ()
  }

otherTeamPrompt :: Prompt
otherTeamPrompt = strPrompt "Other team: " $
  modify . (progMode . gameStateL . otherTeam .~)

homeScorePrompt :: Prompt
homeScorePrompt = numPrompt "Home score: " $
  modify . (progMode . gameStateL . homeScore ?~)

awayScorePrompt :: Prompt
awayScorePrompt = numPrompt "Away score: " $
  modify . (progMode . gameStateL . awayScore ?~)

drawSimplePrompt :: String -> ProgState -> C.Update ()
drawSimplePrompt pStr s = C.drawString $ pStr ++ s ^. inputBuffer
