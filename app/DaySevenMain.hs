module DaySevenMain (main) where

import DaySeven (daySevenPartOne)
import Lib (runOverLinesOfFile)

main :: IO ()
main = do
  putStrLn "day 7 advent of code"
  putStrLn "part 1"
  daySevenPartOneMain

daySevenPartOneMain :: IO ()
daySevenPartOneMain = runOverLinesOfFile "./res/day7-input.txt" daySevenPartOne
