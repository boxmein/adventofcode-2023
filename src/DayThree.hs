module DayThree (
  dayThreePartOne
) where 

import Data.Char (isDigit)

dayThreePartOne :: [String] -> Integer
dayThreePartOne _ = 0

isDot = (== '.')
isSymbol x = (not (isDot x || isDigit x))
