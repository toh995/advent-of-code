module Day10.MainSpec (spec) where

import Data.Functor
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List
import Test.Hspec

import Day10.Main

matrix :: Matrix Tile
matrix =
  buildMatrix
    [ "-L|F7"
    , "7S-7|"
    , "L|7||"
    , "-L-J|"
    , "L|-JF"
    ]

pathCoords :: HashSet Coord
pathCoords =
  HashSet.fromList
    [(1, 1), (1, 2), (1, 3), (2, 3), (3, 3), (3, 2), (3, 1), (2, 1)]

matrix2 :: Matrix Tile
matrix2 =
  buildMatrix
    [ "S------7"
    , "|F----7|"
    , "||....||"
    , "||....||"
    , "|L-7F-J|"
    , "|..||..|"
    , "L--JL--J"
    ]

spec :: Spec
spec = do
  it "coordsForVal works" $ do
    coordsForVal 'S' matrix `shouldBe` [(1, 1)]

  it "lookupM works" $ do
    lookupM matrix (1, 1) `shouldBe` Just 'S'

  it "getStartPos works" $ do
    getStartPos matrix
      `shouldSatisfy` (||)
        <$> (==) (Just ((1, 1), (2, 1)))
        <*> (==) (Just ((1, 1), (1, 2)))

  it "nextPos works" $ do
    nextPos matrix ((1, 3), (1, 2)) `shouldBe` Just ((1, 2), (1, 1))
    nextPos matrix ((1, 2), (1, 1)) `shouldBe` Just ((1, 1), (2, 1))
    nextPos matrix ((1, 1), (2, 1)) `shouldBe` Just ((2, 1), (3, 1))

  it "getContiguousCoords works" $ do
    getContiguousCoords matrix pathCoords (0, 0)
      `shouldBe` HashSet.fromList
        ( map (0,) [0 .. 4]
            ++ map (4,) [0 .. 4]
            ++ map (,0) [0 .. 4]
            ++ map (,4) [0 .. 4]
        )

    let matrix' =
          expand
            . filterCoords (`HashSet.member` pathCoords)
            $ matrix
    let pathCoords' = expandCoords matrix' pathCoords
    getContiguousCoords matrix' pathCoords' (0, 0)
      `shouldBe` HashSet.fromList
        ( ((,) <$> [0, 1, 2] <*> [0 .. 10])
            ++ ((,) <$> [8, 9, 10] <*> [0 .. 10])
            ++ ((,) <$> [0 .. 10] <*> [0, 1, 2])
            ++ ((,) <$> [0 .. 10] <*> [8, 9, 10])
        )
  it "getPathCoords works" $ do
    let expected =
          HashSet.fromList ((,) <$> [0 .. 6] <*> [0 .. 7])
            `HashSet.difference` HashSet.fromList
              [ (2, 2)
              , (2, 3)
              , (2, 4)
              , (2, 5)
              , (3, 2)
              , (3, 3)
              , (3, 4)
              , (3, 5)
              , (5, 1)
              , (5, 2)
              , (5, 5)
              , (5, 6)
              ]
    let actual = getPathCoords matrix2

    (actual <&> sort . HashSet.toList)
      `shouldBe` (Just . sort . HashSet.toList $ expected)

  it "expandCoords works" $ do
    let matrix' = expand matrix
    let actual = expandCoords matrix' pathCoords
    let expected =
          HashSet.fromList
            ( map (3,) [3 .. 7]
                ++ map (7,) [3 .. 7]
                ++ map (,3) [3 .. 7]
                ++ map (,7) [3 .. 7]
            )

    actual `shouldBe` expected

  it "allCoords works" $ do
    (HashSet.fromList . allCoords . expand $ matrix)
      `shouldBe` HashSet.fromList ((,) <$> [0 .. 10] <*> [0 .. 10])

  it "part2 works" $ do
    part2 matrix `shouldBe` Just 1
