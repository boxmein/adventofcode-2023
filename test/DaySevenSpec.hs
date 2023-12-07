module DaySevenSpec (spec) where

import DaySeven
  ( compareHandsByFirstCard,
    daySevenPartOne,
    daySevenPartTwo,
    handsWithRanks,
    handsWithRanksPart2,
    rankOfCard,
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "daySevenPartOne" $ do
    it "works for example" $ do
      daySevenPartOne
        [ "32T3K 765",
          "T55J5 684",
          "KK677 28",
          "KTJJT 220",
          "QQQJA 483"
        ]
        `shouldBe` 6440
  describe "daySevenPartTwo" $ do
    it "works for example" $ do
      daySevenPartTwo
        [ "32T3K 765",
          "T55J5 684",
          "KK677 28",
          "KTJJT 220",
          "QQQJA 483"
        ]
        `shouldBe` 5905
  describe "compareHandsByFirstCard" $ do
    it "compares correctly" $ do
      map
        (uncurry compareHandsByFirstCard)
        [ ("KK677", "KTJJT"),
          ("KTJJT", "QQQJA"),
          ("QQQJA", "T55J5"),
          ("T55J5", "32T3K")
        ]
        `shouldBe` [GT, GT, GT, GT]
  describe "rankOfCard" $ do
    it "ranks correctly" $ do
      map rankOfCard "123456789TJQKA" `shouldBe` [0 .. 13]
  describe "handsWithRanks" $ do
    it "works like example" $ do
      handsWithRanks
        [ ("32T3K", 1),
          ("T55J5", 1),
          ("KK677", 1),
          ("KTJJT", 1),
          ("QQQJA", 1)
        ]
        `shouldBe` [ (("32T3K", 1), 1),
                     (("KTJJT", 1), 2),
                     (("KK677", 1), 3),
                     (("T55J5", 1), 4),
                     (("QQQJA", 1), 5)
                   ]
  describe "handsWithRanksPart2" $ do
    it "works like example" $ do
      handsWithRanksPart2
        [ ("32T3K", 1),
          ("T55J5", 1),
          ("KK677", 1),
          ("KTJJT", 1),
          ("QQQJA", 1)
        ]
        `shouldBe` [ (("32T3K", 1), 1),
                     (("KK677", 1), 2),
                     (("T55J5", 1), 3),
                     (("QQQJA", 1), 4),
                     (("KTJJT", 1), 5)
                   ]
