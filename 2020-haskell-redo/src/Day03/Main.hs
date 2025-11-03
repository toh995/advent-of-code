{-# LANGUAGE NamedFieldPuns #-}

module Day03.Main where

import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import Data.Function ((&))
import Data.Maybe (catMaybes, isJust)

filePath :: String
filePath = "src/Day03/data.txt"

data Tile = Tree | Empty
    deriving (Eq)

main :: IO ()
main = do
    inputStr <- readFile filePath
    let grid = case parseGrid inputStr of
            Right val -> val
            Left errMsg -> error errMsg

    let part1Answer = part1 grid
    putStrLn ("PART 1: " ++ show part1Answer)

    let part2Answer = part2 grid
    putStrLn ("PART 2: " ++ show part2Answer)

parseGrid :: String -> Either String (Grid Tile)
parseGrid s = do
    rows <- s & lines & mapM parseRow
    let rowCount = length rows
    let colCount =
            case rows of
                [] -> 0
                (r : _) -> length r
    let arr =
            A.array
                ((0, 0), (rowCount - 1, colCount - 1))
                [ ((i, j), val)
                | (i, row) <- zip [0 ..] rows
                , (j, val) <- zip [0 ..] row
                ]
    pure Grid{arr}

parseRow :: String -> Either String [Tile]
parseRow = mapM parseTile

parseTile :: Char -> Either String Tile
parseTile '#' = Right Tree
parseTile '.' = Right Empty
parseTile c = Left $ "parse error, invalid char " ++ [c]

part1 :: Grid Tile -> Int
part1 = countTrees (1, 3)

part2 :: Grid Tile -> Int
part2 grid =
    slopes
        & map (`countTrees` grid)
        & product
  where
    slopes =
        [ (1, 1)
        , (1, 3)
        , (1, 5)
        , (1, 7)
        , (2, 1)
        ]

type Slope = (Int, Int)
type Coord = (Int, Int)

countTrees :: Slope -> Grid Tile -> Int
countTrees (di, dj) grid =
    tiles
        & filter (== Tree)
        & length
  where
    coords =
        iterate
            (\(i, j) -> (i + di, j + dj))
            (topLeftCoord grid)
    tiles =
        coords
            & map (grid !?)
            & takeWhile isJust
            & catMaybes

data Grid a = Grid
    { arr :: Array (Int, Int) a
    }

topLeftCoord :: Grid a -> Coord
topLeftCoord Grid{arr} =
    ret
  where
    (ret, _) = A.bounds arr

numCols :: Grid a -> Int
numCols Grid{arr} = maxCol - minCol + 1
  where
    ((_, minCol), (_, maxCol)) = A.bounds arr

(!?) :: Grid a -> (Int, Int) -> Maybe a
(!?) grid@Grid{arr} (i, j) =
    let j' = j `mod` (numCols grid)
     in arr A.!? (i, j')
