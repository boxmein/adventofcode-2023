module Main (main) where

import DayTwo (dayTwoPartOne)
import Lib (runOverLinesOfFile)

main :: IO ()
main = do 
  putStrLn "day 2 advent of code"
  putStrLn "part 1"
  dayTwoPartOneMain
  -- putStrLn "part 2"
  -- dayOnePartTwoMain

dayTwoPartOneMain :: IO () 
dayTwoPartOneMain = runOverLinesOfFile "./res/day2-input.txt" dayTwoPartOne

-- dayOnePartTwoMain :: IO () 
-- dayOnePartTwoMain = runOverLinesOfFile "./res/day1-input.txt" dayOnePartTwo
