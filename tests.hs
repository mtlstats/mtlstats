{-

mtlstats
Copyright (C) 1985, 1996, 2017 Rhéal Lamothe
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
along with this program.  If not, see <http://www.gnu.org/licenses/>.

-}

module Main where

import System.Exit
import Test.HUnit

main :: IO ()
main = do
  counts <- runTestTT tests
  if failures counts > 0 || errors counts > 0
    then do
      putStrLn "Something's fishy!"
      exitFailure
    else putStrLn "Everything's burgery!"

tests :: Test
tests = TestList []
