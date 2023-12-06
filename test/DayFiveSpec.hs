module DayFiveSpec (spec) where

import DayFive
  ( dayFivePartOne,
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "dayFivePartOne" $ do
    it "works like the example" $ do
      dayFivePartOne [] `shouldBe` 0
