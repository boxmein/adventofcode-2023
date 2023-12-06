module DayFourSpec (spec) where

import DayFour
  ( dayFourPartOne,
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "dayFourPartOne" $ do
    it "works like the example" $ do
      dayFourPartOne [] `shouldBe` 0
