module LibSpec (spec) where 

import Test.Hspec

import Lib (stringReplace, splitOnFirst)


spec :: Spec
spec = do 
  describe "stringReplace" $ do
    it "replaces single chars" $ do 
      stringReplace "babababa" "a" "c" `shouldBe` ("bcbababa" :: String)
  describe "splitOnFirst" $ do 
    it "splits on first" $ do 
      splitOnFirst ',' "a, b" `shouldBe` ("a", " b")
    it "ends with empty if empty" $ do 
      splitOnFirst ',' "a," `shouldBe` ("a", "")
    it "ends with empty if no match" $ do 
      splitOnFirst ',' "a" `shouldBe` ("a", "")
