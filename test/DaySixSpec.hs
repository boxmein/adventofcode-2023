module DaySixSpec (spec) where

import DaySix
  ( daySixPartOne,
    daySixPartTwo,
    getRecordBeatingHoldingTimes,
    holdingTimeToDistance,
    parseInput,
    parseInputPart2,
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "holdingTimeToDistance" $ do
    it "maxTime 7" $ do
      holdingTimeToDistance 7
        `shouldBe` [ (0, 0),
                     (1, 6),
                     (2, 10),
                     (3, 12),
                     (4, 12),
                     (5, 10),
                     (6, 6),
                     (7, 0)
                   ]
  describe "getRecordBeatingHoldingTimes" $ do
    it "gets 4 times" $ do
      getRecordBeatingHoldingTimes (7, 9)
        `shouldBe` [ (2, 10),
                     (3, 12),
                     (4, 12),
                     (5, 10)
                   ]
  describe "parseInput" $ do
    it "works on example input" $ do
      parseInput
        [ "Time:      7  15   30",
          "Distance:  9  40  200"
        ]
        `shouldBe` [(7, 9), (15, 40), (30, 200)]
  describe "daySixPartOne" $ do
    it "works for example" $ do
      daySixPartOne
        [ "Time:      7  15   30",
          "Distance:  9  40  200"
        ]
        `shouldBe` 288
  describe "parseInputPart2" $ do
    it "works on example input" $ do
      parseInputPart2
        [ "Time:      7  15   30",
          "Distance:  9  40  200"
        ]
        `shouldBe` (71530, 940200)
  describe "daySixPartTwo" $ do
    it "works for example" $ do
      daySixPartTwo
        [ "Time:      7  15   30",
          "Distance:  9  40  200"
        ]
        `shouldBe` 71503
