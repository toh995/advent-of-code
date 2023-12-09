module Day09.MainSpec (spec) where

import Test.Hspec

import Day09.Main

spec :: Spec
spec = do
  it "nextVal works" $ do
    nextVal ([0, 3, 6, 9, 12, 15] :: [Int]) `shouldBe` Just 18
    nextVal ([1, 3, 6, 10, 15, 21] :: [Int]) `shouldBe` Just 28
    nextVal ([10, 13, 16, 21, 30, 45] :: [Int]) `shouldBe` Just 68

  it "prevVal works" $ do
    prevVal ([0, 3, 6, 9, 12, 15] :: [Int]) `shouldBe` Just (-3)
    prevVal ([1, 3, 6, 10, 15, 21] :: [Int]) `shouldBe` Just 0
    prevVal ([10, 13, 16, 21, 30, 45] :: [Int]) `shouldBe` Just 5
