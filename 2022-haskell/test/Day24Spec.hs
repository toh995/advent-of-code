module Day24Spec (spec) where

import Day24

import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet as HashSet
import Data.List
import Test.Hspec

smallBoardLines :: [Line]
smallBoardLines = [
                      "#.#####"
                    , "#.....#"
                    , "#>....#"
                    , "#.....#"
                    , "#...v.#"
                    , "#.....#"
                    , "#####.#"
                  ]

boardLines :: [Line]
boardLines = [
                  "#.######"
                , "#>>.<^<#"
                , "#.<..<<#"
                , "#>v.><>#"
                , "#<^v^^>#"
                , "######.#"
              ]

board :: Board
board = parseBoard boardLines

spec :: Spec
spec = do
  describe "Parsing" $ do
    it "computes the correct numRows" $ do
      let board' = parseBoard boardLines
      numRows board' `shouldBe` 4

    it "computes the correct numCols" $ do
      let board' = parseBoard boardLines
      numCols board' `shouldBe` 6

    it "computes the correct maxMinutes" $ do
      let board' = parseBoard boardLines
      maxMinutes board' `shouldBe` 12

    it "computes the correct coordToVertices" $ do
      let actual = coordToVertices $ parseBoard smallBoardLines
      let expected = HashMap.fromList [
                                          ((-1,0), HashSet.fromList [Vertex (-1,0) Nothing])
                                        , ((5,4), HashSet.fromList [Vertex (5,4) Nothing])
                                        , ((0,0), HashSet.fromList [Vertex (0,0) (Just 0), Vertex (0,0) (Just 1), Vertex (0,0) (Just 2), Vertex (0,0) (Just 3), Vertex (0,0) (Just 4)])
                                        , ((0,1), HashSet.fromList [Vertex (0,1) (Just 0), Vertex (0,1) (Just 1), Vertex (0,1) (Just 2), Vertex (0,1) (Just 3), Vertex (0,1) (Just 4)])
                                        , ((0,2), HashSet.fromList [Vertex (0,2) (Just 0), Vertex (0,2) (Just 1), Vertex (0,2) (Just 2), Vertex (0,2) (Just 3), Vertex (0,2) (Just 4)])
                                        , ((0,3), HashSet.fromList [Vertex (0,3) (Just 0), Vertex (0,3) (Just 1), Vertex (0,3) (Just 3), Vertex (0,3) (Just 4)])
                                        , ((0,4), HashSet.fromList [Vertex (0,4) (Just 0), Vertex (0,4) (Just 1), Vertex (0,4) (Just 2), Vertex (0,4) (Just 3), Vertex (0,4) (Just 4)])
                                        , ((1,0), HashSet.fromList [Vertex (1,0) (Just 1), Vertex (1,0) (Just 2), Vertex (1,0) (Just 3), Vertex (1,0) (Just 4)])
                                        , ((1,1), HashSet.fromList [Vertex (1,1) (Just 0), Vertex (1,1) (Just 2), Vertex (1,1) (Just 3), Vertex (1,1) (Just 4)])
                                        , ((1,2), HashSet.fromList [Vertex (1,2) (Just 0), Vertex (1,2) (Just 1), Vertex (1,2) (Just 3), Vertex (1,2) (Just 4)])
                                        , ((1,3), HashSet.fromList [Vertex (1,3) (Just 0), Vertex (1,3) (Just 1), Vertex (1,3) (Just 2), Vertex (1,3) (Just 4)])
                                        , ((1,4), HashSet.fromList [Vertex (1,4) (Just 0), Vertex (1,4) (Just 1), Vertex (1,4) (Just 2), Vertex (1,4) (Just 3)])
                                        , ((2,0), HashSet.fromList [Vertex (2,0) (Just 0), Vertex (2,0) (Just 1), Vertex (2,0) (Just 2), Vertex (2,0) (Just 3), Vertex (2,0) (Just 4)])
                                        , ((2,1), HashSet.fromList [Vertex (2,1) (Just 0), Vertex (2,1) (Just 1), Vertex (2,1) (Just 2), Vertex (2,1) (Just 3), Vertex (2,1) (Just 4)])
                                        , ((2,2), HashSet.fromList [Vertex (2,2) (Just 0), Vertex (2,2) (Just 1), Vertex (2,2) (Just 2), Vertex (2,2) (Just 3), Vertex (2,2) (Just 4)])
                                        , ((2,3), HashSet.fromList [Vertex (2,3) (Just 0), Vertex (2,3) (Just 1), Vertex (2,3) (Just 2), Vertex (2,3) (Just 3)])
                                        , ((2,4), HashSet.fromList [Vertex (2,4) (Just 0), Vertex (2,4) (Just 1), Vertex (2,4) (Just 2), Vertex (2,4) (Just 3), Vertex (2,4) (Just 4)])
                                        , ((3,0), HashSet.fromList [Vertex (3,0) (Just 0), Vertex (3,0) (Just 1), Vertex (3,0) (Just 2), Vertex (3,0) (Just 3), Vertex (3,0) (Just 4)])
                                        , ((3,1), HashSet.fromList [Vertex (3,1) (Just 0), Vertex (3,1) (Just 1), Vertex (3,1) (Just 2), Vertex (3,1) (Just 3), Vertex (3,1) (Just 4)])
                                        , ((3,2), HashSet.fromList [Vertex (3,2) (Just 0), Vertex (3,2) (Just 1), Vertex (3,2) (Just 2), Vertex (3,2) (Just 3), Vertex (3,2) (Just 4)])
                                        , ((3,3), HashSet.fromList [Vertex (3,3) (Just 1), Vertex (3,3) (Just 2), Vertex (3,3) (Just 3), Vertex (3,3) (Just 4)])
                                        , ((3,4), HashSet.fromList [Vertex (3,4) (Just 0), Vertex (3,4) (Just 1), Vertex (3,4) (Just 2), Vertex (3,4) (Just 3), Vertex (3,4) (Just 4)])
                                        , ((4,0), HashSet.fromList [Vertex (4,0) (Just 0), Vertex (4,0) (Just 1), Vertex (4,0) (Just 2), Vertex (4,0) (Just 3), Vertex (4,0) (Just 4)])
                                        , ((4,1), HashSet.fromList [Vertex (4,1) (Just 0), Vertex (4,1) (Just 1), Vertex (4,1) (Just 2), Vertex (4,1) (Just 3), Vertex (4,1) (Just 4)])
                                        , ((4,2), HashSet.fromList [Vertex (4,2) (Just 0), Vertex (4,2) (Just 1), Vertex (4,2) (Just 2), Vertex (4,2) (Just 3), Vertex (4,2) (Just 4)])
                                        , ((4,3), HashSet.fromList [Vertex (4,3) (Just 0), Vertex (4,3) (Just 2), Vertex (4,3) (Just 3), Vertex (4,3) (Just 4)])
                                        , ((4,4), HashSet.fromList [Vertex (4,4) (Just 0), Vertex (4,4) (Just 1), Vertex (4,4) (Just 2), Vertex (4,4) (Just 3), Vertex (4,4) (Just 4)])
                                      ]
      actual `shouldBe` expected

    describe "cleanLines" $ do
      it "works" $ do
        let ls = [
                    "#.#####"
                  , "#.....#"
                  , "#>....#"
                  , "#.....#"
                  , "#...v.#"
                  , "#.....#"
                  , "#####.#"
                 ]
        cleanLines ls `shouldBe` [
                                     "....."
                                   , ">...."
                                   , "....."
                                   , "...v."
                                   , "....."
                                 ]

    it "getBlizzards works" $ do
      let actual = getBlizzards . cleanLines $ boardLines
      let expected = [
                         Blizzard (0,0) East
                       , Blizzard (0,1) East
                       , Blizzard (0,3) West
                       , Blizzard (0,4) North
                       , Blizzard (0,5) West
                       , Blizzard (1,1) West
                       , Blizzard (1,4) West
                       , Blizzard (1,5) West
                       , Blizzard (2,0) East
                       , Blizzard (2,1) South
                       , Blizzard (2,3) East
                       , Blizzard (2,4) West
                       , Blizzard (2,5) East
                       , Blizzard (3,0) West
                       , Blizzard (3,1) North
                       , Blizzard (3,2) South
                       , Blizzard (3,3) North
                       , Blizzard (3,4) North
                       , Blizzard (3,5) East
                     ]
      sort actual `shouldBe` sort expected

    -- it "buildMinuteToOpenSpaces works" $ do
    --   let actual = buildMinuteToOpenSpaces 4 6 12 $ cleanLines boardLines
    --   let expected = HashMap.fromList [
    --                                       (0, [(0,3), (1,0), (1,2), (1,3), (2,2)])
    --                                     , (1, [(0,0), (0,3), (0,5), (1,1), (1,2), (1,5), (2,2), (2,5), (3,2), (3,3)])
    --                                     , (2, [(0,0), (0,4), (0,5), (1,0), (2,0), (2,3), (3,0), (3,2), (3,3), (3,5)])
    --                                     , (3, [()])
    --                                     , (4, [])
    --                                     , (5, [])
    --                                     , (6, [])
    --                                     , (7, [])
    --                                     , (8, [])
    --                                     , (9, [])
    --                                     , (10, [])
    --                                     , (11, [])
    --                                   ]
    --   actual `shouldBe` expected

    it "buildMinuteToBlizzards works" $ do
      let actual = buildMinuteToBlizzards 5 5 5 $ cleanLines smallBoardLines
      let expected = HashMap.fromList [
                                          (0, [Blizzard (1,0) East, Blizzard (3,3) South])
                                        , (1, [Blizzard (1,1) East, Blizzard (4,3) South])
                                        , (2, [Blizzard (1,2) East, Blizzard (0,3) South])
                                        , (3, [Blizzard (1,3) East, Blizzard (1,3) South])
                                        , (4, [Blizzard (1,4) East, Blizzard (2,3) South])
                                      ]
      actual `shouldBe` expected

    -- it "getCoords works" $ do
    --   let actual = getCoords 2 3
    --   let expected = [(0,0), (0,1), (0,2), (1,0), (1,1), (1,2)]
    --   sort actual `shouldBe` sort expected

  describe "remove utils" $ do
    it "can removeFirst" $ do
      removeFirst [1,2] `shouldBe` ([2] :: [Int])
      removeFirst [2] `shouldBe` ([] :: [Int])
      removeFirst [] `shouldBe` ([] :: [Int])

    it "can removeLast" $ do
      removeLast [1,2] `shouldBe` ([1] :: [Int])
      removeLast [2] `shouldBe` ([] :: [Int])
      removeLast [] `shouldBe` ([] :: [Int])

    it "can removeFirstAndLast" $ do
      removeFirstAndLast [1,2,3] `shouldBe` ([2] :: [Int])
      removeFirstAndLast [1,2] `shouldBe` ([] :: [Int])
      removeFirstAndLast [2] `shouldBe` ([] :: [Int])
      removeFirstAndLast [] `shouldBe` ([] :: [Int])

  describe "manhattanDistance" $ do
    it "works" $ do
      manhattanDistance (1,1) (0,0) `shouldBe` 2

  describe "moveBlizzard" $ do
    it "moves properly" $ do
      moveBlizzard (numRows board) (numCols board) (Blizzard (1,1) North) `shouldBe` Blizzard (0,1)  North
      moveBlizzard (numRows board) (numCols board) (Blizzard (1,1) South) `shouldBe` Blizzard (2,1)  South
      moveBlizzard (numRows board) (numCols board) (Blizzard (1,1) East) `shouldBe` Blizzard (1,2)  East
      moveBlizzard (numRows board) (numCols board) (Blizzard (1,1) West) `shouldBe` Blizzard (1,0)  West

    it "moves on the edges properly" $ do
      let iMax = numRows board - 1
      let jMax = numCols board - 1
      moveBlizzard (numRows board) (numCols board) (Blizzard (0,0) North) `shouldBe` Blizzard (iMax,0)  North
      moveBlizzard (numRows board) (numCols board) (Blizzard (iMax,jMax) South) `shouldBe` Blizzard (0,jMax)  South
      moveBlizzard (numRows board) (numCols board) (Blizzard (iMax,jMax) East) `shouldBe` Blizzard (iMax,0)  East
      moveBlizzard (numRows board) (numCols board) (Blizzard (0,0) West) `shouldBe` Blizzard (0,jMax)  West

  describe "getNeighbors" $ do
    it "works on a middle coord" $ do
      let actual = getNeighbors board $ Vertex (1,1) (Just 1)
      let expected = HashSet.fromList [
                                        --   Vertex (0,1) (Just 5)
                                        -- , Vertex (0,1) (Just 9)
                                        -- , Vertex (2,1) (Just 6)
                                        -- , Vertex (2,1) (Just 11)
                                        -- , Vertex (1,2) (Just 0)
                                        -- , Vertex (1,2) (Just 1)
                                        -- , Vertex (1,2) (Just 4)
                                        -- , Vertex (1,2) (Just 7)
                                        -- , Vertex (1,0) (Just 0)
                                        Vertex (1,0) (Just 2)
                                        -- , Vertex (1,0) (Just 2)
                                        -- , Vertex (1,0) (Just 3)
                                        -- , Vertex (1,0) (Just 6)
                                        -- , Vertex (1,0) (Just 8)
                                        -- , Vertex (1,0) (Just 9)
                                      ]
      actual `shouldBe` expected

    it "works on an edge coord" $ do
      let actual = getNeighbors board $ Vertex (3,5) (Just 1)
      let expected = HashSet.fromList [
                                        --   Vertex (2,5) (Just 1)
                                        -- , Vertex (2,5) (Just 3)
                                        -- , Vertex (2,5) (Just 4)
                                        -- , Vertex (2,5) (Just 7)
                                        -- , Vertex (2,5) (Just 9)
                                        -- , Vertex (2,5) (Just 10)
                                        -- , Vertex (3,4) (Just 3)
                                        -- , Vertex (3,4) (Just 6)
                                        -- , Vertex (3,4) (Just 7)
                                        -- , Vertex (3,4) (Just 10)
                                        -- , Vertex (4,5) Nothing
                                        Vertex (3,5) (Just 2)
                                        , Vertex (4,5) Nothing
                                      ]
      actual `shouldBe` expected


  describe "edgeWeight" $ do
    it "returns Nothing for non-adjacent vertices" $ do
      let from = Vertex (1,1) Nothing
      let to = Vertex (0,0) Nothing
      edgeWeight board from to `shouldBe` Nothing

    it "returns the to value, if from has no minutes" $ do
      let from = Vertex (0,1) Nothing
      let to = Vertex (0,0) $ Just 32
      edgeWeight board from to `shouldBe` Just 32

    it "returns 1, if to has no minutes" $ do
      let from = Vertex (0,0) $ Just 32
      let to = Vertex (0,1) Nothing
      edgeWeight board from to `shouldBe` Just 1

    it "returns maxMinutes, if they are adjacent, but same minutes" $ do
      let from = Vertex (0,0) $ Just 1
      let to = Vertex (0,1) $ Just 1
      edgeWeight board from to `shouldBe` Just (maxMinutes board)

    it "returns correct value, if from minutes < to minutes" $ do
      let from = Vertex (0,1) $ Just 1
      let to = Vertex (0,0) $ Just 9
      edgeWeight board from to `shouldBe` Just 8

    it "returns correct value, if from minutes > to minutes" $ do
      let from = Vertex (0,0) $ Just 9
      let to = Vertex (0,1) $ Just 1
      edgeWeight board from to `shouldBe` Just 4

  it "part 1 works" $ do
    part1 board `shouldBe` 18

  it "part 2 works" $ do
    part2 board `shouldBe` Just 54
