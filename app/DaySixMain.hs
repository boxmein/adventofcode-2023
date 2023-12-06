module DaySixMain (main) where

import DaySix (daySixPartOne, daySixPartTwo)
import Lib (runOverLinesOfFile)

main :: IO ()
main = do
  putStrLn "day 6 advent of code"
  putStrLn "part 1"
  daySixPartOneMain
  putStrLn "part 2"
  daySixPartTwoMain

daySixPartOneMain :: IO ()
daySixPartOneMain = runOverLinesOfFile "./res/day6-input.txt" daySixPartOne

daySixPartTwoMain :: IO ()
daySixPartTwoMain = runOverLinesOfFile "./res/day6-input.txt" daySixPartTwo
