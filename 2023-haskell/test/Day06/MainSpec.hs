module Day06.MainSpec (spec) where

import Test.Hspec

import Day06.Main

spec :: Spec
spec = do
  describe "getDistance" $ do
    it "works" $ do
      getDistance 7 0 `shouldBe` 0
      getDistance 7 1 `shouldBe` 6
      getDistance 7 2 `shouldBe` 10
      getDistance 7 3 `shouldBe` 12
      getDistance 7 4 `shouldBe` 12
      getDistance 7 5 `shouldBe` 10
      getDistance 7 6 `shouldBe` 6
      getDistance 7 7 `shouldBe` 0

  describe "numWaysToWin" $ do
    it "works" $ do
      numWaysToWin Race{totalTime = 7, targetDistance = 9} `shouldBe` 4
      numWaysToWin Race{totalTime = 15, targetDistance = 40} `shouldBe` 8
      numWaysToWin Race{totalTime = 30, targetDistance = 200} `shouldBe` 9
      numWaysToWin Race{totalTime = 71530, targetDistance = 940200} `shouldBe` 71503

  describe "part1" $ do
    it "works" $ do
      let expected = 288
      let actual =
            computeAnswer
              [ Race{totalTime = 7, targetDistance = 9}
              , Race{totalTime = 15, targetDistance = 40}
              , Race{totalTime = 30, targetDistance = 200}
              ]
      actual `shouldBe` expected
