module DayFourMain (main) where

import DayFour (dayFourPartOne)
import Lib (runOverLinesOfFile)

main :: IO ()
main = do
  putStrLn "day 4 advent of code"
  putStrLn "part 1"
  dayFourPartOneMain

dayFourPartOneMain :: IO ()
dayFourPartOneMain = runOverLinesOfFile "./res/day4-input.txt" dayFourPartOne
