module Day17.MainSpec (spec) where

import Test.Hspec

import Day17.Main

matrix :: Matrix Int
matrix =
  buildMatrix
    [ [2, 4, 1, 3, 4, 3, 2, 3, 1, 1, 3, 2, 3]
    , [3, 2, 1, 5, 4, 5, 3, 5, 3, 5, 6, 2, 3]
    , [3, 2, 5, 5, 2, 4, 5, 6, 5, 4, 2, 5, 4]
    , [3, 4, 4, 6, 5, 8, 5, 8, 4, 5, 4, 5, 2]
    , [4, 5, 4, 6, 6, 5, 7, 8, 6, 7, 5, 3, 6]
    , [1, 4, 3, 8, 5, 9, 8, 7, 9, 8, 4, 5, 4]
    , [4, 4, 5, 7, 8, 7, 6, 9, 8, 7, 7, 6, 6]
    , [3, 6, 3, 7, 8, 7, 7, 9, 7, 9, 6, 5, 3]
    , [4, 6, 5, 4, 9, 6, 7, 9, 8, 6, 8, 8, 7]
    , [4, 5, 6, 4, 6, 7, 9, 9, 8, 6, 4, 5, 3]
    , [1, 2, 2, 4, 6, 8, 6, 8, 6, 5, 5, 6, 3]
    , [2, 5, 4, 6, 5, 4, 8, 8, 8, 7, 7, 3, 5]
    , [4, 3, 2, 2, 6, 7, 4, 6, 5, 5, 5, 3, 3]
    ]

spec :: Spec
spec = do
  it "getCost works" $ do
    getCost matrix ((0, 0), D) ((0, 2), R) `shouldBe` 5
    getCost matrix ((0, 2), R) ((1, 2), D) `shouldBe` 1
    getCost matrix ((1, 2), D) ((1, 5), R) `shouldBe` 14
    getCost matrix ((1, 5), R) ((0, 5), U) `shouldBe` 3
    getCost matrix ((0, 5), U) ((0, 8), R) `shouldBe` 6
    getCost matrix ((0, 8), R) ((2, 8), D) `shouldBe` 8
    getCost matrix ((2, 8), D) ((2, 10), R) `shouldBe` 6
    getCost matrix ((2, 10), R) ((4, 10), D) `shouldBe` 9
    getCost matrix ((4, 10), D) ((4, 11), R) `shouldBe` 3
    getCost matrix ((4, 11), R) ((7, 11), D) `shouldBe` 16
    getCost matrix ((7, 11), D) ((7, 12), R) `shouldBe` 3
    getCost matrix ((7, 12), R) ((10, 12), D) `shouldBe` 13
    getCost matrix ((10, 12), D) ((10, 11), L) `shouldBe` 6
    getCost matrix ((10, 11), L) ((12, 11), D) `shouldBe` 6
    getCost matrix ((12, 11), D) ((12, 12), R) `shouldBe` 3
