module Day05.Part01Spec (spec) where

import GHC.Data.Maybe
import Test.Hspec
import Text.Megaparsec

import Day05.Part01

catMap :: CatMap
catMap =
  CatMap
    [ CatMapItem{sourceStart = 98, destStart = 50, rangeLen = 2}
    , CatMapItem{sourceStart = 50, destStart = 52, rangeLen = 48}
    ]

spec :: Spec
spec = do
  describe "convert" $ do
    it "works" $ do
      convert 79 catMap `shouldBe` 81
      convert 14 catMap `shouldBe` 14
      convert 55 catMap `shouldBe` 57
      convert 13 catMap `shouldBe` 13
      convert 98 catMap `shouldBe` 50
      convert 99 catMap `shouldBe` 51

  describe "catMapP" $ do
    it "works" $ do
      let inputStr =
            "seed-to-soil map:\n"
              ++ "50 98 2\n"
              ++ "52 50 48"
      let parsedMaybe =
            rightToMaybe
              . runParser catMapP ""
              $ inputStr
      let expected =
            Just $
              CatMap
                [ CatMapItem{sourceStart = 98, destStart = 50, rangeLen = 2}
                , CatMapItem{sourceStart = 50, destStart = 52, rangeLen = 48}
                ]
      parsedMaybe `shouldBe` expected
