-- Used solution: https://github.com/villuna/aoc23/wiki/A-Geometric-solution-to-advent-of-code-2023,-day-21
module Day21.Part02 where

import Data.Array.IArray (Array)
import Data.Array.IArray qualified as A
import Data.List (partition)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq

filePath :: String
filePath = "src/Day21/data.txt"

main :: IO ()
main = do
    matrix <- parseMatrix <$> readFile filePath
    let part2Answer = part2 matrix
    putStrLn $ "PART 2: " ++ show part2Answer

type Matrix = Array Coord Tile
type Coord = (Int, Int)

data Tile = StartTile | FullTile | EmptyTile
    deriving (Eq)

part2 :: Matrix -> Int
part2 matrix =
    sum
        [ ((n + 1) ^ (2 :: Int)) * length oddDists
        , (n ^ (2 :: Int)) * length evenDists
        , negate $ (n + 1) * length oddCornerDists
        , n * length evenCornerDists
        ]
  where
    n = 26_501_365 `div` 131
    startCoord =
        head
            [ coord
            | (coord, tile) <- A.assocs matrix
            , tile == StartTile
            ]
    distMap = distancesFrom startCoord matrix
    (evenDists, oddDists) = partition even (M.elems distMap)
    evenCornerDists = filter (> 65) evenDists
    oddCornerDists = filter (> 65) oddDists

distancesFrom :: Coord -> Matrix -> Map Coord Int
distancesFrom startCoord matrix =
    bfs
        (Seq.singleton (startCoord, 0))
        M.empty
  where
    bfs Empty acc = acc
    bfs ((coord, stepCount) :<| queue) acc
        | coord `M.member` acc = bfs queue acc
        | otherwise =
            case matrix A.!? coord of
                Just EmptyTile -> bfs (queue >< nextQueue) nextAcc
                Just StartTile -> bfs (queue >< nextQueue) nextAcc
                Just FullTile -> bfs queue acc
                Nothing -> bfs queue acc
      where
        nextQueue =
            Seq.fromList
                [ (neighbor, stepCount + 1)
                | neighbor <- neighborCoords coord
                ]
        nextAcc = M.insert coord stepCount acc

neighborCoords :: Coord -> [Coord]
neighborCoords (i, j) =
    [ (i + 1, j)
    , (i - 1, j)
    , (i, j + 1)
    , (i, j - 1)
    ]

parseMatrix :: String -> Matrix
parseMatrix s =
    A.array
        ((0, 0), (maxI, maxJ))
        [ ((i, j), tile)
        | (i, row) <- zip [0 ..] rows
        , (j, tile) <- zip [0 ..] row
        ]
  where
    rows =
        map (map parseTile) $
            lines s
    maxI = length rows - 1
    maxJ = length (head rows) - 1

parseTile :: Char -> Tile
parseTile '#' = FullTile
parseTile '.' = EmptyTile
parseTile 'S' = StartTile
parseTile c = error $ "invalid tile character " ++ [c]
