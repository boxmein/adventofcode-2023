module DayFour
  ( dayFourPartOne,
    parseRow,
    score,
    ScratchCard (ScratchCard),
  )
where

import Data.List (intersect)

-- ID, winning, yours
data ScratchCard = ScratchCard Integer [Integer] [Integer] deriving (Show, Eq)

dayFourPartOne :: [String] -> Integer
dayFourPartOne input =
  sum (map (score . parseRow) input)

score :: ScratchCard -> Integer
score card =
  let countValue = (countWinningNumbers card)
   in if countValue == 0 then 0 else 2 ^ (countValue - 1)

countWinningNumbers :: ScratchCard -> Integer
countWinningNumbers (ScratchCard _ winning yours) = toInteger (length (intersect winning yours))

-- Card #: Winning | Yours
-- Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
parseRow :: String -> ScratchCard
parseRow s =
  let ws = drop 1 (words s)
      cardId = read (init (head ws)) :: Integer
      winning = map read (takeWhile (/= "|") (drop 1 ws)) :: [Integer]
      yours = map read (drop 1 (dropWhile (/= "|") ws)) :: [Integer]
   in ScratchCard cardId winning yours
