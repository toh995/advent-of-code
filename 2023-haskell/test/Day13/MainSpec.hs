module Day13.MainSpec (spec) where

import Test.Hspec

import Day13.Main

spec :: Spec
spec = do
  it "isReflection works" $ do
    let list1 =
          [ "#...##..#"
          , "#....#..#"
          , "..##..###"
          , "#####.##."
          ]
    let list2 =
          [ "#####.##."
          , "..##..###"
          , "#....#..#"
          ]
    isReflection list1 list2 `shouldBe` True

  it "getPatternScore1 works" $ do
    getPatternScore1
      [ ".#..####..#"
      , "#....##...."
      , "..##....##."
      , "#..#....#.."
      , "##..####..#"
      , ".#..####..#"
      , "#####..####"
      ]
      `shouldBe` 6

    getPatternScore1
      [ "###.###.#.#"
      , "...##......"
      , "..#########"
      , "###.#......"
      , "##....#####"
      , "......#..##"
      , "###....#..."
      , "...##....##"
      , "##...#..###"
      ]
      `shouldBe` 1

    getPatternScore1
      [ ".##....####"
      , "...#.###..."
      , ".##..#.#.#."
      , "#....#...##"
      , "#...###.###"
      , ".##.#.#...."
      , ".##.#.#...."
      , "#...###.###"
      , "#....#...##"
      , ".##..#.#.#."
      , "...#.###..."
      , ".##....####"
      , "#.##.#..##."
      , "..#.####..#"
      , "....####..#"
      ]
      `shouldBe` 600

  it "getPatternScore2 works" $ do
    getPatternScore2
      [ "#.##..##."
      , "..#.##.#."
      , "##......#"
      , "##......#"
      , "..#.##.#."
      , "..##..##."
      , "#.#.##.#."
      ]
      `shouldBe` 300
