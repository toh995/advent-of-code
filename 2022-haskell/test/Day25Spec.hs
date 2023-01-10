module Day25Spec (spec) where

import Day25

import Test.Hspec

sampleLines :: [Line]
sampleLines = [
      "1=-0-2"
    , "12111"
    , "2=0="
    , "21"
    , "2=01"
    , "111"
    , "20012"
    , "112"
    , "1=-1="
    , "1-12"
    , "12"
    , "1="
    , "122"
  ]

spec :: Spec
spec = do
  it "snafuToDecimal works" $ do
    snafuToDecimal "1" `shouldBe` 1
    snafuToDecimal "2" `shouldBe` 2
    snafuToDecimal "1=" `shouldBe` 3
    snafuToDecimal "1-" `shouldBe` 4
    snafuToDecimal "10" `shouldBe` 5
    snafuToDecimal "11" `shouldBe` 6
    snafuToDecimal "12" `shouldBe` 7
    snafuToDecimal "2=" `shouldBe` 8
    snafuToDecimal "2-" `shouldBe` 9
    snafuToDecimal "20" `shouldBe` 10
    snafuToDecimal "1=0" `shouldBe` 15
    snafuToDecimal "1-0" `shouldBe` 20
    snafuToDecimal "1=11-2" `shouldBe` 2022
    snafuToDecimal "1-0---0" `shouldBe` 12345
    snafuToDecimal "1121-1110-1=0" `shouldBe` 314159265
    snafuToDecimal "1=-0-2 " `shouldBe` 1747
    snafuToDecimal "12111" `shouldBe` 906
    snafuToDecimal "2=0=" `shouldBe` 198
    snafuToDecimal "21" `shouldBe` 11
    snafuToDecimal "2=01" `shouldBe` 201
    snafuToDecimal "111" `shouldBe` 31
    snafuToDecimal "20012" `shouldBe` 1257
    snafuToDecimal "112" `shouldBe` 32
    snafuToDecimal "1=-1=" `shouldBe` 353
    snafuToDecimal "1-12" `shouldBe` 107
    snafuToDecimal "12" `shouldBe` 7
    snafuToDecimal "1=" `shouldBe` 3
    snafuToDecimal "122" `shouldBe` 37

  it "decimalToSnafu works" $ do
    decimalToSnafu 1 `shouldBe` "1"
    decimalToSnafu 2 `shouldBe` "2"
    decimalToSnafu 3 `shouldBe` "1="
    decimalToSnafu 4 `shouldBe` "1-"
    decimalToSnafu 5 `shouldBe` "10"
    decimalToSnafu 6 `shouldBe` "11"
    decimalToSnafu 7 `shouldBe` "12"
    decimalToSnafu 8 `shouldBe` "2="
    decimalToSnafu 9 `shouldBe` "2-"
    decimalToSnafu 10 `shouldBe` "20"
    decimalToSnafu 15 `shouldBe` "1=0"
    decimalToSnafu 20 `shouldBe` "1-0"
    decimalToSnafu 2022 `shouldBe` "1=11-2"
    decimalToSnafu 12345 `shouldBe` "1-0---0"
    decimalToSnafu 314159265 `shouldBe` "1121-1110-1=0"
    decimalToSnafu 1747 `shouldBe` "1=-0-2"
    decimalToSnafu 906 `shouldBe` "12111"
    decimalToSnafu 198 `shouldBe` "2=0="
    decimalToSnafu 11 `shouldBe` "21"
    decimalToSnafu 201 `shouldBe` "2=01"
    decimalToSnafu 31 `shouldBe` "111"
    decimalToSnafu 1257 `shouldBe` "20012"
    decimalToSnafu 32 `shouldBe` "112"
    decimalToSnafu 353 `shouldBe` "1=-1="
    decimalToSnafu 107 `shouldBe` "1-12"
    decimalToSnafu 7 `shouldBe` "12"
    decimalToSnafu 3 `shouldBe` "1="
    decimalToSnafu 37 `shouldBe` "122"

  it "part1 works" $ do
    part1 sampleLines == "2=-1=0"
