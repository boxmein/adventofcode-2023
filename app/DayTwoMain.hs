module Main (main) where

import DayTwo (dayTwoPartOne, dayTwoPartTwo)
import Lib (runOverLinesOfFile)

main :: IO ()
main = do 
  putStrLn "day 2 advent of code"
  putStrLn "part 1"
  dayTwoPartOneMain
  putStrLn "part 2"
  dayTwoPartTwoMain

dayTwoPartOneMain :: IO () 
dayTwoPartOneMain = runOverLinesOfFile "./res/day2-input.txt" dayTwoPartOne

dayTwoPartTwoMain :: IO () 
dayTwoPartTwoMain = runOverLinesOfFile "./res/day2-input.txt" dayTwoPartTwo
