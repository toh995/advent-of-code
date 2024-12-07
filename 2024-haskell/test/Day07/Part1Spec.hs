module Day07.Part1Spec (spec) where

import Test.Hspec

import Day07.Part1

spec :: Spec
spec = do
  describe "permuteOperators" $ do
    it "works" $ do
      permuteOperators 2 `shouldBe` [[Add, Add], [Add, Mul], [Mul, Add], [Mul, Mul]]
