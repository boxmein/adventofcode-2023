module DayFiveMain (main) where

import DayFive (dayFivePartOne)
import Lib (runOverLinesOfFile)

main :: IO ()
main = do
  putStrLn "day 5 advent of code"
  putStrLn "part 1"
  dayFivePartOneMain

dayFivePartOneMain :: IO ()
dayFivePartOneMain = runOverLinesOfFile "./res/day5-input.txt" dayFivePartOne
