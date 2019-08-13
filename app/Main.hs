module Main where

import Control.Monad.Trans.State (evalStateT)
import UI.NCurses (runCurses)

import Mtlstats

main :: IO ()
main = runCurses $ initState >>= evalStateT mainLoop
