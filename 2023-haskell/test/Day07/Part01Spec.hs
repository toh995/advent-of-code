module Day07.Part01Spec (spec) where

import Data.List
import Test.Hspec

import Day07.Part01

spec :: Spec
spec = do
  it "Card ordering works" $ do
    Two < Three `shouldBe` True
    A < Two `shouldBe` False
    A > Two `shouldBe` True

  it "getType works" $ do
    getType Hand{cards = [A, A, A, A, A], bid = 0} `shouldBe` FiveOfAKind
    getType Hand{cards = [A, J, A, A, A], bid = 0} `shouldBe` FourOfAKind
    getType Hand{cards = [A, Ten, A, A, Ten], bid = 0} `shouldBe` FullHouse
    getType Hand{cards = [A, Ten, A, A, Q], bid = 0} `shouldBe` ThreeOfAKind
    getType Hand{cards = [A, Ten, A, Ten, Q], bid = 0} `shouldBe` TwoPair
    getType Hand{cards = [A, Ten, A, Nine, Q], bid = 0} `shouldBe` OnePair
    getType Hand{cards = [A, Ten, Three, Nine, Q], bid = 0} `shouldBe` HighCard

  it "Hand ordering works" $ do
    let hand1 = Hand{cards = [Ten, Ten, Nine, Ten, Nine], bid = 71}
    let hand2 = Hand{cards = [J, J, J, J, J], bid = 739}
    hand1 < hand2 `shouldBe` True
    sort [hand2, hand1] `shouldBe` [hand1, hand2]
