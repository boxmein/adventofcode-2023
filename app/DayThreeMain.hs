module Main (main) where

import DayThree (dayThreePartOne)
import Lib (runOverLinesOfFile)

main :: IO ()
main = do 
  putStrLn "day 3 advent of code"
  putStrLn "part 1"
  dayThreePartOneMain
  -- putStrLn "part 2"
  -- dayThreePartTwoMain

dayThreePartOneMain :: IO () 
dayThreePartOneMain = runOverLinesOfFile "./res/day3-input.txt" dayThreePartOne

-- dayThreePartTwoMain :: IO () 
-- dayThreePartTwoMain = runOverLinesOfFile "./res/day2-input.txt" dayThreePartTwo
