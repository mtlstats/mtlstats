{- |

mtlstats
Copyright (C) 1984, 1985, 2019, 2020, 2021 Rhéal Lamothe
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

module Mtlstats.Control.TitleScreen (titleScreenC) where

import Control.Monad.Trans.State (modify)
import Data.Char (chr)
import qualified UI.NCurses as C

import Mtlstats.Actions
import Mtlstats.Format
import Mtlstats.Types

titleScreenC :: Controller
titleScreenC = Controller
  { drawController = const $ do
    (_, cols) <- C.windowSize
    C.drawString $ unlines $ map (centre $ fromIntegral $ pred cols)
      $  [ ""
         , "MONTREAL CANADIENS STATISTICS"
         ]
      ++ titleText
      ++ [ ""
         , "Copyright (C) 1984, 1985, 2019, 2020, 2021 Rhéal Lamothe"
         , "<rheal.lamothe@gmail.com>"
         , ""
         , "Press any key to continue..."
         ]
    return C.CursorInvisible
  , handleController = \case
    C.EventCharacter _  -> modify backHome >> return True
    C.EventSpecialKey _ -> modify backHome >> return True
    _                   -> return True
  }

titleText :: [String]
titleText = box $ map (map blockify) $ foldl joinBlocks (repeat "")
  [chM, chT, chL, chS, chT, chA, chT, chS]

box :: [String] -> [String]
box strs
  =  [[tl] ++ replicate width horiz ++ [tr]]
  ++ map (\str -> [vert] ++ str ++ [vert]) strs
  ++ [[bl] ++ replicate width horiz ++ [br]]
  where
    width = length $ head strs
    tl    = chr 0x2554
    tr    = chr 0x2557
    bl    = chr 0x255a
    br    = chr 0x255d
    horiz = chr 0x2550
    vert  = chr 0x2551

blockify :: Char -> Char
blockify = \case
  '#' -> chr 0x2588
  '>' -> chr 0x2590
  '<' -> chr 0x258c
  ch  -> ch

joinBlocks :: [String] -> [String] -> [String]
joinBlocks = zipWith (++)

chM :: [String]
chM =
  [ "##<   >##"
  , ">##   ##<"
  , ">##< >##<"
  , ">### ###<"
  , ">#######<"
  , ">#<###>#<"
  , ">#<>#<>#<"
  , "##<   >##"
  ]

chT :: [String]
chT =
  [ ">########<"
  , ">## ## ##<"
  , ">#< ## >#<"
  , "    ##    "
  , "    ##    "
  , "    ##    "
  , "    ##    "
  , "   >##<   "
  ]

chL :: [String]
chL =
  [ "###    "
  , ">#<    "
  , ">#<    "
  , ">#<    "
  , ">#<    "
  , ">#<  ##"
  , ">#< >##"
  , "#######"
  ]

chS :: [String]
chS =
  [ " #####< "
  , ">#<  ## "
  , "##      "
  , " #####< "
  , "     >#<"
  , "      ##"
  , ">#<  >#<"
  , " ###### "
  ]

chA :: [String]
chA =
  [ "  >##<  "
  , "   ##   "
  , "  >##<  "
  , "  ####  "
  , " >#<>#< "
  , " ###### "
  , ">#<  >#<"
  , "###  ###"
  ]
