module Day01.Part02Spec (spec) where

import Data.Maybe
import Test.Hspec

import Day01.Part02
import Util.Types

inputLines :: [Line]
inputLines =
  [ "two1nine"
  , "eightwothree"
  , "abcone2threexyz"
  , "xtwone3four"
  , "4nineeightseven2"
  , "zoneight234"
  , "7pqrstsixteen"
  ]

spec :: Spec
spec = do
  describe "extractInt" $ do
    it "extracts the correct integers" $ do
      mapMaybe extractInt inputLines `shouldBe` [29, 83, 13, 24, 42, 14, 76]

  describe "part2" $ do
    it "computes the correct sum" $ do
      part2 inputLines `shouldBe` 281

  describe "extractDigits" $ do
    it "works" $ do
      extractDigits "dljxl7five6nrzfh5one" `shouldBe` "75651"
