module DaySix
  ( daySixPartOne,
    daySixPartTwo,
    holdingTimeToDistance,
    getRecordBeatingHoldingTimes,
    parseInput,
    parseInputPart2,
  )
where

daySixPartOne :: [String] -> Integer
daySixPartOne = (foldl (*) 1) . (map countRecordBeatingHoldingTimes) . parseInput

daySixPartTwo :: [String] -> Integer
daySixPartTwo = countRecordBeatingHoldingTimes . parseInputPart2

parseInput :: [String] -> [(Integer, Integer)]
parseInput [times, distances] = zip (parseRow times) (parseRow distances)

parseInputPart2 :: [String] -> (Integer, Integer)
parseInputPart2 [times, distances] = (parseRowPart2 times, parseRowPart2 distances)

parseRow :: String -> [Integer]
parseRow = map read . drop 1 . words

parseRowPart2 :: String -> Integer
parseRowPart2 = read . concat . drop 1 . words

distanceTravelled :: Integer -> Integer -> Integer
distanceTravelled raceTime holdingTime = holdingTime * (raceTime - holdingTime)

holdingTimeToDistance :: Integer -> [(Integer, Integer)]
holdingTimeToDistance maxTime = map (mapSnd (distanceTravelled maxTime) . dupe) [0 .. maxTime]

getRecordBeatingHoldingTimes :: (Integer, Integer) -> [(Integer, Integer)]
getRecordBeatingHoldingTimes (maxTime, recordDistance) = filter ((> recordDistance) . snd) (holdingTimeToDistance maxTime)

countRecordBeatingHoldingTimes :: (Integer, Integer) -> Integer
countRecordBeatingHoldingTimes = (toInteger . length) . getRecordBeatingHoldingTimes

dupe :: Integer -> (Integer, Integer)
dupe a = (a, a)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (a, b) = (a, f b)
