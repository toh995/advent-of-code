{-# LANGUAGE NamedFieldPuns #-}

module Day04.Main where

import Control.Category ((>>>))
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

filePath :: FilePath
filePath = "src/Day04/data.txt"

main :: IO ()
main = do
    grid <-
        readFile filePath
            <&> (lines >>> newGrid)
    let part1Answer = part1 grid
    putStrLn $ "PART1: " ++ show part1Answer
    let part2Answer = part2 grid
    putStrLn $ "PART2: " ++ show part2Answer

part1 :: Grid -> Int
part1 = length . accessiblePaperCoords

part2 :: Grid -> Int
part2 gridInit = go gridInit 0
  where
    go grid acc
        | null accessiblePaperCoords' = acc
        | otherwise =
            go
                nextGrid
                (acc + length accessiblePaperCoords')
      where
        accessiblePaperCoords' = accessiblePaperCoords grid
        nextGrid =
            foldr
                deletePaperCoord
                grid
                accessiblePaperCoords'

paperVal :: Char
paperVal = '@'

type Coord = (Int, Int)

newtype Grid = Grid {neighborCounts :: Map Coord Int}

newGrid :: [[Char]] -> Grid
newGrid rows =
    Grid
        { neighborCounts =
            M.fromList
                [ (coord, numNeighbors)
                | coord <- toList paperCoords
                , let numNeighbors =
                        S.size $
                            S.intersection
                                (S.fromList $ neighborCoords coord)
                                paperCoords
                ]
        }
  where
    paperCoords =
        S.fromList
            [ (i, j)
            | (i, row) <- zip [0 ..] rows
            , (j, val) <- zip [0 ..] row
            , val == paperVal
            ]

accessiblePaperCoords :: Grid -> [Coord]
accessiblePaperCoords Grid{neighborCounts} =
    [ coord
    | (coord, count) <- M.assocs neighborCounts
    , count < 4
    ]

deletePaperCoord :: Coord -> Grid -> Grid
deletePaperCoord coord Grid{neighborCounts} =
    Grid{neighborCounts = neighborCounts''}
  where
    neighborCounts' = M.delete coord neighborCounts
    neighborCounts'' =
        foldr
            (M.adjust $ subtract 1)
            neighborCounts'
            (neighborCoords coord)

neighborCoords :: (Int, Int) -> [(Int, Int)]
neighborCoords (i, j) =
    [ (i + di, j + dj)
    | di <- [-1, 0, 1]
    , dj <- [-1, 0, 1]
    , (di, dj) /= (0, 0)
    ]
