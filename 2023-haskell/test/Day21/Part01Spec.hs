module Day21.Part01Spec (spec) where

import Test.Hspec

import Day21.Part01

matrix :: Matrix Char
matrix =
  buildMatrix
    [ "..........."
    , ".....###.#."
    , ".###.##..#."
    , "..#.#...#.."
    , "....#.#...."
    , ".##..S####."
    , ".##..#...#."
    , ".......##.."
    , ".##.#.####."
    , ".##..##.##."
    , "..........."
    ]

spec :: Spec
spec = do
  it "part1 works" $ do
    let expected = 16
    let actual = part1 matrix 6
    actual `shouldBe` expected
