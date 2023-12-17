module Day14.MainSpec (spec) where

import Data.Maybe
import Test.Hspec

import Day14.Main

matrix :: Matrix Tile
matrix =
  parseMatrix $
    "O....#....\n"
      ++ "O.OO#....#\n"
      ++ ".....##...\n"
      ++ "OO.#O....O\n"
      ++ ".O.....O#.\n"
      ++ "O.#..O.#.#\n"
      ++ "..O..#O..O\n"
      ++ ".......O..\n"
      ++ "#....###..\n"
      ++ "#OO..#....\n"

slidMatrix :: Matrix Tile
slidMatrix =
  parseMatrix $
    "OOOO.#.O..\n"
      ++ "OO..#....#\n"
      ++ "OO..O##..O\n"
      ++ "O..#.OO...\n"
      ++ "........#.\n"
      ++ "..#....#.#\n"
      ++ "..O..#.O.O\n"
      ++ "..O.......\n"
      ++ "#....###..\n"
      ++ "#....#....\n"

spec :: Spec
spec = do
  it "parseMatrix works" $ do
    matrix `shouldNotBe` emptyMatrix

  it "slideAll works" $ do
    let actual = slideAll U matrix
    actual `shouldBe` slidMatrix
    actual `shouldNotBe` emptyMatrix

  it "computeLoad works" $ do
    computeLoad slidMatrix `shouldBe` 136

  it "revolve works" $ do
    let actual = take 4 $ iterate revolve matrix
    let m1 =
          parseMatrix $
            ".....#....\n"
              ++ "....#...O#\n"
              ++ "...OO##...\n"
              ++ ".OO#......\n"
              ++ ".....OOO#.\n"
              ++ ".O#...O#.#\n"
              ++ "....O#....\n"
              ++ "......OOOO\n"
              ++ "#...O###..\n"
              ++ "#..OO#....\n"
    let m2 =
          parseMatrix $
            ".....#....\n"
              ++ "....#...O#\n"
              ++ ".....##...\n"
              ++ "..O#......\n"
              ++ ".....OOO#.\n"
              ++ ".O#...O#.#\n"
              ++ "....O#...O\n"
              ++ ".......OOO\n"
              ++ "#..OO###..\n"
              ++ "#.OOO#...O\n"
    let m3 =
          parseMatrix $
            ".....#....\n"
              ++ "....#...O#\n"
              ++ ".....##...\n"
              ++ "..O#......\n"
              ++ ".....OOO#.\n"
              ++ ".O#...O#.#\n"
              ++ "....O#...O\n"
              ++ ".......OOO\n"
              ++ "#...O###.O\n"
              ++ "#.OOO#...O\n"
    actual `shouldBe` [matrix, m1, m2, m3]
    emptyMatrix `notElem` actual `shouldBe` True

  it "findFirstDupeIdxes works" $ do
    findFirstDupeIdxes ([1, 1] :: [Int]) `shouldBe` Just (0, 1)
    findFirstDupeIdxes ([1, 2, 1] :: [Int]) `shouldBe` Just (0, 2)
    findFirstDupeIdxes ([1, 2, 3, 1] :: [Int]) `shouldBe` Just (0, 3)
    findFirstDupeIdxes ([0, 1, 2, 3, 1] :: [Int]) `shouldBe` Just (1, 4)

  it "part2 works" $ do
    part2 matrix `shouldBe` Just 64

  it "check index math" $ do
    let matrices = iterate revolve matrix
    let (i, i') =
          fromMaybe
            (-1, -1)
            (findFirstDupeIdxes matrices)
    let cycleLen = i' - i
    matrices !! 10 `shouldBe` matrices !! (((10 - i) `mod` cycleLen) + i)
    matrices !! 11 `shouldBe` matrices !! (((11 - i) `mod` cycleLen) + i)
