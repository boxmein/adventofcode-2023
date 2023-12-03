module DayTwo (
  dayTwoPartOne,
  isGrabPossible,
  isPossible,
  parseGrabToSegments,
  parseGrab,
  parseGrabs,
  parseGame,
  Grab (Grab),
  Game (Game)
) where 

import Data.List (find)
import Data.Maybe (catMaybes)

-- grab: red cubes, green cubes, blue cubes
data Grab = Grab Integer Integer Integer deriving (Show, Eq)
-- game: id, grabs
data Game = Game Integer [Grab] deriving (Show, Eq)

-- "Game \d+: (((\d+) (blue|red|green)){3};)*((\d+) (blue|red|green)){3}"
parseGame :: String -> Game 
parseGame s = let
    start = drop 5 s
    gameIdStr = takeWhile (/= ':') start 
    gameIdInt = read gameIdStr :: Integer
    afterColon = drop 2 (dropWhile (/= ':') start)
    grabs = parseGrabs afterColon
  in 
    Game gameIdInt grabs

isPossible :: Integer -> Integer -> Integer -> Game -> Bool
isPossible maxR maxG maxB (Game _ grabs) = all (isGrabPossible maxR maxG maxB) grabs

isGrabPossible :: Integer -> Integer -> Integer -> Grab -> Bool
isGrabPossible maxR maxG maxB (Grab r g b) = r <= maxR && g <= maxG && b <= maxB 


unwrapOrZero :: Maybe Integer -> Integer
unwrapOrZero (Just x) = x
unwrapOrZero Nothing = 0

parseGrabs :: String -> [Grab]
parseGrabs = catMaybes . parseGrabsMaybe

parseGrabsMaybe :: String -> [Maybe Grab]
parseGrabsMaybe s = 
  let 
    firstSegment = takeWhile (/= ';') s
    rest = drop 2 (dropWhile (/= ';') s)
  in 
    (parseGrab firstSegment) : (if rest == "" then [] else (parseGrabsMaybe rest))

parseGrab :: String -> Maybe Grab 
parseGrab s = 
  let segments = parseGrabToSegments s
      r = unwrapOrZero (fmap fst (find ((=="red").snd) segments))
      g = unwrapOrZero (fmap fst (find ((=="green").snd) segments))
      b = unwrapOrZero (fmap fst (find ((=="blue").snd) segments))
  in
    Just (Grab r g b)


parseGrabToSegments :: String -> [(Integer, String)]
parseGrabToSegments s = 
    let 
      segment = takeWhile (/= ',') s
    in 
      if 
        segment == "" 
        then 
          [] 
          else 
            let 
              rest = drop 2 (dropWhile (/= ',') s)
              first = takeWhile (/= ' ') segment
              secondItem = drop 1 (dropWhile (/= ' ') segment)
              parsedFirst = (read first) :: Integer
            in
              (parsedFirst, secondItem) : parseGrabToSegments rest 

getGameId :: Game -> Integer 
getGameId (Game i _) = i

dayTwoPartOne :: [String] -> Integer
dayTwoPartOne input = 
  let 
    games = map parseGame input 
    possibleGames = filter (isPossible 12 13 14) games
  in 
    sum (map getGameId possibleGames)

