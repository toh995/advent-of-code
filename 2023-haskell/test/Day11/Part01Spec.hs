module Day11.Part01Spec (spec) where

import Data.HashSet qualified as HashSet
import Test.Hspec

import Day11.Part01

spec :: Spec
spec = do
  it "expand works" $ do
    let input =
          [ "...#......"
          , ".......#.."
          , "#........."
          , ".........."
          , "......#..."
          , ".#........"
          , ".........#"
          , ".........."
          , ".......#.."
          , "#...#....."
          ]
    let expectedOutput =
          [ "....#........"
          , ".........#..."
          , "#............"
          , "............."
          , "............."
          , "........#...."
          , ".#..........."
          , "............#"
          , "............."
          , "............."
          , ".........#..."
          , "#....#......."
          ]
    expand input `shouldBe` expectedOutput

  it "manhattanDistance works" $ do
    manhattanDistance (6, 1) (11, 5) `shouldBe` 9
    manhattanDistance (11, 5) (6, 1) `shouldBe` 9

  it "parseGalaxyCoords works" $ do
    let input =
          [ "....#........"
          , ".........#..."
          , "#............"
          , "............."
          , "............."
          , "........#...."
          , ".#..........."
          , "............#"
          , "............."
          , "............."
          , ".........#..."
          , "#....#......."
          ]
    let coords = parseGalaxyCoords input
    HashSet.fromList coords
      `shouldBe` HashSet.fromList [(0, 4), (1, 9), (2, 0), (5, 8), (6, 1), (7, 12), (10, 9), (11, 0), (11, 5)]

  it "getAllPairs works" $ do
    (length . getAllPairs $ ([1 .. 9] :: [Int])) `shouldBe` 36
