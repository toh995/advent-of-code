module Day04.MainSpec (spec) where

import Data.List
import Test.Hspec

import Day04.Main

input :: String
input =
  unlines
    [ "MMMSXXMASM"
    , "MSAMXMSMSA"
    , "AMXSXMAAMM"
    , "MSAMASMSMX"
    , "XMASAMXAMM"
    , "XXAMMXXAMA"
    , "SMSMSASXSS"
    , "SAXAMASAAA"
    , "MAMMMXMMMM"
    , "MXMXAXMASX"
    ]

spec :: Spec
spec = do
  describe "countMatches" $ do
    it "counts both XMAS and SAMX" $ do
      countMatches "MMMSXXMASM" `shouldBe` 1
      countMatches (reverse "MSAMXMSMSA") `shouldBe` 1
      countMatches "XMASAMXAMM" `shouldBe` 1
      countMatches
        "SMASMXMXSAMXSASXXAMXMAXXXMSXMASAMAXMASXSMXXMXMXAMXXXSAMXMXMSMXSXMASMSAAAXMASXMSMMSSXMMSSMXSMXXXMAXMMMXMXAMXMXAXMASXMMMXAXMAMSAMXAMMAAXXMASAM"
        `shouldBe` 6

  describe "search spaces" $ do
    it "extracts rows properly" $ do
      lines input
        `shouldBe` [ "MMMSXXMASM"
                   , "MSAMXMSMSA"
                   , "AMXSXMAAMM"
                   , "MSAMASMSMX"
                   , "XMASAMXAMM"
                   , "XXAMMXXAMA"
                   , "SMSMSASXSS"
                   , "SAXAMASAAA"
                   , "MAMMMXMMMM"
                   , "MXMXAXMASX"
                   ]

    it "extracts columns properly" $ do
      (transpose . lines $ input)
        `shouldBe` [ "MMAMXXSSMM"
                   , "MSMSMXMAAX"
                   , "MAXAAASXMM"
                   , "SMSMSMMAMX"
                   , "XXXAAMSMMA"
                   , "XMMSMXAAXX"
                   , "MSAMXXSSMM"
                   , "AMASAAXAMA"
                   , "SSMMMMSAMS"
                   , "MAMXMASAMX"
                   ]
