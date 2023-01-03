module Day23Spec (spec) where

import Day23

import qualified Data.HashSet as HashSet
import GHC.Utils.Misc hiding (Direction)
import Test.Hspec

boardLines :: [Line]
boardLines = [
      "....#.."
    , "..###.#"
    , "#...#.#"
    , ".#...##"
    , "#.###.."
    , "##.#.##"
    , ".#..#.."
  ]

board :: Board
board = parseBoard boardLines

smallBoard :: Board
smallBoard = parseBoard [
      "....."
    , "..##."
    , "..#.."
    , "....."
    , "..##."
    , "....."
  ]

largeBoard :: Board
largeBoard = parseBoard [
    ".............."
  , ".............."
  , ".......#......"
  , ".....###.#...."
  , "...#...#.#...."
  , "....#...##...."
  , "...#.###......"
  , "...##.#.##...."
  , "....#..#......"
  , ".............."
  , ".............."
  , ".............."
  ]

spec :: Spec
spec = do
  describe "Parsing" $ do
    it "can parse a board correctly" $ do
      let actual = parseBoard boardLines 
      let expected = Board $ HashSet.fromList [(0,4), (1,2), (1,3), (1,4), (1,6), (2,0),
                                               (2,4), (2,6), (3,1), (3,5), (3,6), (4,0),
                                               (4,2), (4,3), (4,4), (5,0), (5,1), (5,3),
                                               (5,5), (5,6), (6,1), (6,4)]
      expected `shouldBe` actual

  describe "coordInBoard" $ do
    it "can tell if a coordinate exists in the board" $ do
      coordInBoard (0,4) board `shouldBe` True

    it "can tell if a coordinate doesn't exist in the board" $ do
      coordInBoard (-124, -21) board `shouldBe` False

  describe "hasAnyNeighbor" $ do
    it "can tell if a neighboring coordinate exists in the board" $ do
      hasAnyNeighbor (2,1) board `shouldBe` True
      
    it "can tell if a neighboring coordinate doesn't exist in the board" $ do
      hasAnyNeighbor (0,0) board `shouldBe` False

  describe "getNeighborlyCoords" $ do
    it "can give a list of coordinates that have a neighbor in the board" $ do
      getNeighborlyCoords board `shouldBe` getCoords board

  describe "Getting neighbors" $ do
    it "can get neighbors correctly" $ do
      let actual = HashSet.fromList $ getNeighbors (0,0)
      let expected = HashSet.fromList [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]
      expected `shouldBe` actual

    it "can get the North neighbor correctly" $ do
      let actual = getNeighbor (0,0) North
      let expected = (-1,0)
      expected `shouldBe` actual

    it "can get the South neighbor correctly" $ do
      let actual = getNeighbor (0,0) South
      let expected = (1,0)
      expected `shouldBe` actual

    it "can get the East neighbor correctly" $ do
      let actual = getNeighbor (0,0) East
      let expected = (0,1)
      expected `shouldBe` actual

    it "can get the West neighbor correctly" $ do
      let actual = getNeighbor (0,0) West
      let expected = (0,-1)
      expected `shouldBe` actual

    it "can get the NorthEast neighbor correctly" $ do
      let actual = getNeighbor (0,0) NorthEast
      let expected = (-1,1)
      expected `shouldBe` actual

    it "can get the NorthWest neighbor correctly" $ do
      let actual = getNeighbor (0,0) NorthWest
      let expected = (-1,-1)
      expected `shouldBe` actual

    it "can get the SouthEast neighbor correctly" $ do
      let actual = getNeighbor (0,0) SouthEast
      let expected = (1,1)
      expected `shouldBe` actual

    it "can get the SouthWest neighbor correctly" $ do
      let actual = getNeighbor (0,0) SouthWest
      let expected = (1,-1)
      expected `shouldBe` actual

  describe "Making proposals" $ do
    it "succeeds in proposing North" $ do
      let coords = [(1,2), (1,3), (4,2), (4,3)]
      let actual = map
                     (makeProposalWithDirs smallBoard initialDirections)
                     coords
      let expected = map
                       (\c -> Just $ MoveProposal {startCoord=c, endCoord=getNeighbor c North})
                       coords
      expected `shouldBe` actual

    it "succeeds in proposing South" $ do
      let startCoord = (2,2)
      let endCoord = (3,2)
      let actual = makeProposalWithDirs smallBoard initialDirections (2,2)
      let expected = Just $ MoveProposal {startCoord, endCoord}
      expected `shouldBe` actual

  describe "removeDupeEndCoords" $ do
    it "removes duplicate end coords" $ do
      let endCoord = (2,3)
      let proposals = [MoveProposal (1,1) endCoord, MoveProposal (2,2) endCoord]
      removeDupeEndCoords proposals `shouldBe` []

    it "keeps unique end coords" $ do
      let proposals = [MoveProposal (1,1) (1,2), MoveProposal (2,2) (3,4)]
      removeDupeEndCoords proposals `shouldBe` proposals

  describe "Do round" $ do
    it "successfully completes one round on the small board" $ do
      let (actual, _, _, _) = nTimes 1 doRound (smallBoard, initialDirections, 0, Nothing)
      let expected = parseBoard [
                         "..##."
                       , "....."
                       , "..#.."
                       , "...#."
                       , "..#.."
                       , "....."
                     ]
      expected `shouldBe` actual

    it "successfully completes two rounds on the small board" $ do
      let (actual, _, _, _) = nTimes 2 doRound (smallBoard, initialDirections, 0, Nothing)
      let expected = parseBoard [
                         "....."
                       , "..##."
                       , ".#..."
                       , "....#"
                       , "....."
                       , "..#.."
                     ]
      expected `shouldBe` actual

    it "successfully completes three rounds on the small board" $ do
      let (actual, _, _, _) = nTimes 3 doRound (smallBoard, initialDirections, 0, Nothing)
      let expected = parseBoard [
                          "..#.."
                        , "....#"
                        , "#...."
                        , "....#"
                        , "....."
                        , "..#.."
                        , "....."
                     ]
      expected `shouldBe` actual

    it "succesfully completes ten rounds on the large board" $ do
      let (actual, _, _, _) = nTimes 10 doRound (largeBoard, initialDirections, 0, Nothing)
      let expected = parseBoard [
                          ".......#......"
                        , "...........#.."
                        , "..#.#..#......"
                        , "......#......."
                        , "...#.....#..#."
                        , ".#......##...."
                        , ".....##......."
                        , "..#........#.."
                        , "....#.#..#...."
                        , ".............."
                        , "....#..#..#..."
                        , ".............."
                     ]
      expected `shouldBe` actual

  describe "max and min for board" $ do
    let board' = parseBoard [
              ".......#......"
            , "...........#.."
            , "..#.#..#......"
            , "......#......."
            , "...#.....#..#."
            , ".#......##...."
            , ".....##......."
            , "..#........#.."
            , "....#.#..#...."
            , ".............."
            , "....#..#..#..."
            , ".............."
         ]

    it "gets the correct i max" $ do
      getMaxI board' `shouldBe` Just 10
      
    it "gets the correct i min" $ do
      getMinI board' `shouldBe` Just 0

    it "gets the correct j max" $ do
      getMaxJ board' `shouldBe` Just 12
      
    it "gets the correct j min" $ do
      getMinJ board' `shouldBe` Just 1

  describe "getRectangleArea" $ do
    it "computes the correct answer" $ do
      let board' = parseBoard [
                ".......#......"
              , "...........#.."
              , "..#.#..#......"
              , "......#......."
              , "...#.....#..#."
              , ".#......##...."
              , ".....##......."
              , "..#........#.."
              , "....#.#..#...."
              , ".............."
              , "....#..#..#..."
              , ".............."
           ]
      getRectangleArea board' `shouldBe` Just (12 * 11)

  describe "part1" $ do
    it "computes the correct answer" $ do
      part1 largeBoard `shouldBe` Just 110

  describe "part2" $ do
    it "computes the correct answer" $ do
      part2 largeBoard `shouldBe` 20
