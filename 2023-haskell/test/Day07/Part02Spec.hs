module Day07.Part02Spec (spec) where

import Data.List
import Test.Hspec

import Day07.Part02

spec :: Spec
spec = do
  it "Card ordering works" $ do
    Two < Three `shouldBe` True
    A < Two `shouldBe` False
    A > Two `shouldBe` True

  it "Hand ordering works" $ do
    let hand1 = Hand{cards = [Ten, Ten, Nine, Ten, Nine], bid = 71}
    let hand2 = Hand{cards = [J, J, J, J, J], bid = 739}
    hand1 < hand2 `shouldBe` True
    sort [hand2, hand1] `shouldBe` [hand1, hand2]

  it "getType works" $ do
    getType Hand{cards = [A, A, A, A, A], bid = 0} `shouldBe` FiveOfAKind
    getType Hand{cards = [A, Q, A, A, A], bid = 0} `shouldBe` FourOfAKind
    getType Hand{cards = [A, Ten, A, A, Ten], bid = 0} `shouldBe` FullHouse
    getType Hand{cards = [A, Ten, A, A, Q], bid = 0} `shouldBe` ThreeOfAKind
    getType Hand{cards = [A, Ten, A, Ten, Q], bid = 0} `shouldBe` TwoPair
    getType Hand{cards = [A, Ten, A, Nine, Q], bid = 0} `shouldBe` OnePair
    getType Hand{cards = [A, Ten, Three, Nine, Q], bid = 0} `shouldBe` HighCard

    -- Test J's
    -- 1 J
    getType Hand{cards = [J, Q, K, A, Two], bid = 0} `shouldBe` OnePair
    getType Hand{cards = [J, Q, Q, A, Two], bid = 0} `shouldBe` ThreeOfAKind
    getType Hand{cards = [J, Q, Q, Q, Two], bid = 0} `shouldBe` FourOfAKind
    getType Hand{cards = [J, Q, Q, A, A], bid = 0} `shouldBe` FullHouse
    getType Hand{cards = [J, Q, Q, Q, Q], bid = 0} `shouldBe` FiveOfAKind
    -- 2 J's
    getType Hand{cards = [J, J, Q, A, Two], bid = 0} `shouldBe` ThreeOfAKind
    getType Hand{cards = [J, J, Q, Q, Two], bid = 0} `shouldBe` FourOfAKind
    getType Hand{cards = [J, J, Q, Q, Q], bid = 0} `shouldBe` FiveOfAKind
    -- 3 J's
    getType Hand{cards = [J, J, J, A, Two], bid = 0} `shouldBe` FourOfAKind
    getType Hand{cards = [J, J, J, A, A], bid = 0} `shouldBe` FiveOfAKind
    -- 4 J's
    getType Hand{cards = [J, J, J, J, Two], bid = 0} `shouldBe` FiveOfAKind
    -- 5 J's
    getType Hand{cards = [J, J, J, J, J], bid = 0} `shouldBe` FiveOfAKind
