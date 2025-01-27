module Day10.MainSpec (spec) where

import Data.Functor
import Data.HashSet qualified as HashSet
import Test.Hspec

import Day10.Main
import Day10.Parse

spec :: Spec
spec = do
  describe "findApexCoords" $ do
    it "works" $ do
      let inputStr =
            "0090009\n"
              ++ "0001098\n"
              ++ "0002007\n"
              ++ "6543456\n"
              ++ "7650987\n"
              ++ "8760000\n"
              ++ "9870000\n"
      (parseMatrix inputStr <&> (`findApexCoords` (0, 3)))
        `shouldBe` Right (HashSet.fromList [(0, 6), (1, 5), (4, 4), (6, 0)])
