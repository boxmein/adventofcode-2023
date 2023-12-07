module DaySeven
  ( daySevenPartOne,
    daySevenPartTwo,
    handsWithRanks,
    handsWithRanksPart2,
    compareHandsByFirstCard,
    rankOfCard,
  )
where

import Data.List (elemIndex, find, group, maximumBy, sort, sortBy)
import Data.Maybe (fromJust, isNothing)

daySevenPartOne :: [String] -> Integer
daySevenPartOne input =
  let rows = map parseRow input
      ranked = handsWithRanks rows
   in -- winnings : sum(bet * rank)
      sum (map (\((_, rank), bet) -> bet * rank) ranked)

daySevenPartTwo :: [String] -> Integer
daySevenPartTwo input =
  let rows = map parseRow input
      ranked = handsWithRanksPart2 rows
   in -- winnings : sum(bet * rank)
      sum (map (\((_, rank), bet) -> bet * rank) ranked)

-- "card bet"
parseRow :: String -> (String, Integer)
parseRow row =
  let w = words row
      card = head w
      bet = read (w !! 1) :: Integer
   in (card, bet)

handsWithRanks :: [(String, Integer)] -> [((String, Integer), Integer)]
handsWithRanks hands = zip (sortBy sortHands hands) [1 ..]

handsWithRanksPart2 :: [(String, Integer)] -> [((String, Integer), Integer)]
handsWithRanksPart2 hands = zip (sortBy sortHandsPart2 hands) [1 ..]

-- sortHands ("AAAAA",1) ("99999",2) -> LT
-- sortHands "AAAAA" "AAAAA" -> EQ
-- sortHands "QQQTT" "TT999" - LT
sortHands :: (String, Integer) -> (String, Integer) -> Ordering
sortHands (a, _) (b, _) =
  let ra = rankOf a
      rb = rankOf b
   in if ra > rb
        then LT
        else
          if ra < rb
            then GT
            else compareHandsByFirstCard a b

-- use part 2 ranking and hand-comparison
sortHandsPart2 :: (String, Integer) -> (String, Integer) -> Ordering
sortHandsPart2 (a, _) (b, _) =
  let ra = rankOfPart2 a
      rb = rankOfPart2 b
   in if ra > rb
        then LT
        else
          if ra < rb
            then GT
            else compareHandsByFirstCardPart2 a b

compareHandsByFirstCard :: String -> String -> Ordering
compareHandsByFirstCard a b = map rankOfCard a `compare` map rankOfCard b

compareHandsByFirstCardPart2 :: String -> String -> Ordering
compareHandsByFirstCardPart2 a b = map rankOfCardPart2 a `compare` map rankOfCardPart2 b

rankOfCard :: Char -> Int
rankOfCard = fromJust . flip elemIndex "123456789TJQKA"

rankOfCardPart2 :: Char -> Int
rankOfCardPart2 = fromJust . flip elemIndex "J123456789TQKA"

rankOf :: String -> Int
rankOf hand =
  fromJust
    ( elemIndex
        True
        [ isFiveOfAKind isCountCombination hand,
          isFourOfAKind isCountCombination hand,
          isFullHouse isCountCombination hand,
          isThreeOfAKind isCountCombination hand,
          isTwoPair isCountCombination hand,
          isOnePair isCountCombination hand,
          isHighCard isCountCombination hand
        ]
    )

rankOfPart2 :: String -> Int
rankOfPart2 hand =
  fromJust
    ( elemIndex
        True
        [ isFiveOfAKind isCountCombinationPart2 hand,
          isFourOfAKind isCountCombinationPart2 hand,
          isFullHouse isCountCombinationPart2 hand,
          isThreeOfAKind isCountCombinationPart2 hand,
          isTwoPair isCountCombinationPart2 hand,
          isOnePair isCountCombinationPart2 hand,
          isHighCard isCountCombinationPart2 hand
        ]
    )

cardCounts :: String -> [Integer]
cardCounts = map snd . cardCountsWithType

cardCountsWithType :: String -> [(Char, Integer)]
cardCountsWithType = sort . map (\xs@(x : _) -> (x, toInteger (length xs))) . group . sort

cardCountsPart2 :: String -> [Integer]
cardCountsPart2 s =
  let originalCounts = cardCountsWithType s
      rankOfTuple (c, _) = rankOfCardPart2 c
      compareByTuple a b = rankOfTuple a `compare` rankOfTuple b
      bestCard = maximumBy compareByTuple originalCounts
      jokers = (find (\(c, _) -> c == 'J') originalCounts)
      jokerCount = if isNothing jokers then 0 else snd (fromJust jokers)
   in (sort . map snd) (filter (\(c, _) -> c /= 'J') originalCounts ++ [(fst bestCard, snd bestCard + jokerCount)])

isCountCombinationPart2 :: [Integer] -> String -> Bool
isCountCombinationPart2 s = (== s) . cardCountsPart2

isCountCombination :: [Integer] -> String -> Bool
isCountCombination s = (== s) . cardCounts

isHighCard :: ([Integer] -> String -> Bool) -> String -> Bool
isHighCard c = c [1, 1, 1, 1, 1]

isOnePair :: ([Integer] -> String -> Bool) -> String -> Bool
isOnePair c = c [1, 1, 1, 2]

isTwoPair :: ([Integer] -> String -> Bool) -> String -> Bool
isTwoPair c = c [1, 2, 2]

isThreeOfAKind :: ([Integer] -> String -> Bool) -> String -> Bool
isThreeOfAKind c = c [1, 1, 3]

isFullHouse :: ([Integer] -> String -> Bool) -> String -> Bool
isFullHouse c = c [2, 3]

isFourOfAKind :: ([Integer] -> String -> Bool) -> String -> Bool
isFourOfAKind c = c [1, 4]

isFiveOfAKind :: ([Integer] -> String -> Bool) -> String -> Bool
isFiveOfAKind c = c [5]
