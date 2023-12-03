module DayTwoSpec (spec) where 

import Test.Hspec

import DayTwo (dayTwoPartOne,
  isGrabPossible,
  isPossible,
  parseGrabToSegments,
  parseGrab,
  parseGrabs,
  parseGame,
  Grab (Grab),
  Game (Game))

spec :: Spec
spec = do 
  describe "isGrabPossible" $ do 
    it "is possible if all elements are lower than max" $ do 
      isGrabPossible 2 2 2 (Grab 1 1 1) `shouldBe` True
    it "is possible if all elements are equal to max" $ do 
      isGrabPossible 2 2 2 (Grab 2 2 2) `shouldBe` True
  describe "isPossible" $ do
    it "is possible if all grabs are below the maximum" $ do 
      isPossible 12 13 14 (Game 1 [Grab 4 0 3, Grab 1 2 6, Grab 0 2 0]) `shouldBe` True
    it "is not possible if any grab's any element is above the maximum" $ do 
      isPossible 12 13 14 (Game 1 [Grab 4 0 3, Grab 1 2 15, Grab 0 2 0]) `shouldBe` False
  describe "parseGrabToSegments" $ do 
    it "works on example data" $ do 
      parseGrabToSegments "3 blue, 4 red" `shouldBe` [(3,"blue"),(4,"red")]
  describe "parseGrab" $ do 
    it "works on full specifications" $ do 
      parseGrab "1 red, 2 green, 3 blue" `shouldBe` Just (Grab 1 2 3)
    it "works on partial specifications" $ do 
      parseGrab "1 red, 2 green, 3 blue" `shouldBe` Just (Grab 1 2 3)
      parseGrab "1 red, 3 blue" `shouldBe` Just (Grab 1 0 3)
      parseGrab "1 red, 2 green" `shouldBe` Just (Grab 1 2 0)
      parseGrab "2 green, 3 blue" `shouldBe` Just (Grab 0 2 3)
      parseGrab "1 red" `shouldBe` Just (Grab 1 0 0 )
      parseGrab "2 green" `shouldBe` Just (Grab 0 2 0)
      parseGrab "3 blue" `shouldBe` Just (Grab 0 0 3)
    it "returns Grab 0 0 0 on emptystring" $ do 
      parseGrab "" `shouldBe` Just (Grab 0 0 0)

  describe "parseGrabs" $ do 
    it "parses 1 grab" $ do 
      parseGrabs "1 red, 2 green, 3 blue" `shouldBe` [Grab 1 2 3]

    it "parses examples" $ do 
      parseGrabs "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" `shouldBe` [Grab 4 0 3, Grab 1 2 6, Grab 0 2 0]

  describe "parseGame" $ do
    it "parses examples" $ do 
      parseGame "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" `shouldBe` (Game 1 [Grab 4 0 3, Grab 1 2 6, Grab 0 2 0])
      parseGame "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red" `shouldBe` (Game 3 [Grab 20 8 6, Grab 4 13 5, Grab 1 5 0])
      parseGame "Game 62: 1 green, 8 red, 8 blue; 11 blue, 2 red; 1 green, 10 blue, 12 red; 7 red, 2 blue, 1 green; 6 red, 1 green, 11 blue; 1 green, 6 red, 6 blue" `shouldBe` (Game 62 [Grab 8 1 8, Grab 2 0 11, Grab 12 1 10, Grab 7 1 2, Grab 6 1 11, Grab 6 1 6])
  
  describe "dayTwoPartOne" $ do 
    it "works like the example" $ do 
      dayTwoPartOne [
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
        "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
        "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
        "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"] `shouldBe` 8
