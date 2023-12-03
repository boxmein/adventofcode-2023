module DayTwo (
  dayTwoPartOne,
  dayTwoPartTwo,
  isGrabPossible,
  isPossible,
  parseGrabToSegments,
  parseGrab,
  parseGrabs,
  parseGame,
  minCubes,
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
      unwrapColor color = unwrapOrZero (fmap fst (find ((==color) . snd) segments))
      r = unwrapColor "red"
      g = unwrapColor "green"
      b = unwrapColor "blue"
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
dayTwoPartOne = sum . map getGameId . filter (isPossible 12 13 14) . map parseGame

minCubes :: Game -> (Integer, Integer, Integer)
minCubes (Game _ grabs) = 
  let 
    takeR (Grab r _ _) = r 
    takeG (Grab _ g _) = g 
    takeB (Grab _ _ b) = b
    maxOf f = maximum $ map f grabs
  in 
    (maxOf takeR, maxOf takeG, maxOf takeB)

dayTwoPartTwo :: [String] -> Integer
dayTwoPartTwo = sum . map ((\(a,b,c)->a*b*c) . minCubes . parseGame) 
