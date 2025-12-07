{-# LANGUAGE MultiWayIf #-}

module Day07.Main where

import Data.Array (Array)
import qualified Data.Array as A
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Ix (Ix, range)
import qualified Data.Set as S

filePath :: FilePath
filePath = "src/Day07/data.txt"

main :: IO ()
main = do
    inputStr <- readFile filePath
    let grid =
            inputStr
                & lines
                & toGrid
    let part1Answer = part1 grid
    putStrLn $ "PART1: " ++ show part1Answer
    let part2Answer = part2 grid
    putStrLn $ "PART2: " ++ show part2Answer

type Grid a = Array (Int, Int) a

toGrid :: [[a]] -> Grid a
toGrid [] = A.array ((1, 1), (0, 0)) []
toGrid rows@(firstRow : _) =
    A.array
        arrBounds
        [ ((i, j), val)
        | (i, row) <- zip [0 ..] rows
        , (j, val) <- zip [0 ..] row
        ]
  where
    arrBounds = ((0, 0), (numRows - 1, numCols - 1))
    numRows = length rows
    numCols = length firstRow

splitterChar :: Char
splitterChar = '^'

startChar :: Char
startChar = 'S'

part1 :: Grid Char -> Int
part1 grid =
    go 1 startJs 0
  where
    ((_, minJ), (maxI, maxJ)) = A.bounds grid
    startJs =
        S.fromList
            [ j
            | j <- [minJ .. maxJ]
            , grid A.! (0, j) == startChar
            ]
    go i js acc
        | i > maxI = acc
        | otherwise =
            go
                (i + 1)
                newJs
                (acc + S.size splitterJs)
      where
        (splitterJs, nonSplitterJs) =
            S.partition
                (\j -> grid A.! (i, j) == splitterChar)
                js
        newJs =
            S.union
                nonSplitterJs
                ( S.fromList $
                    concat
                        [ [j - 1, j + 1]
                        | j <- toList splitterJs
                        ]
                )

part2 :: Grid Char -> Int
part2 grid =
    countTimelines initialCoord
  where
    initialCoord =
        head
            [ (0, j)
            | j <- [minJ .. maxJ]
            , grid A.! (0, j) == startChar
            ]
    ((_, minJ), (maxI, maxJ)) = A.bounds grid
    countTimelines =
        memoize
            (A.bounds grid)
            $ \(i, j) ->
                if
                    | i == maxI -> 1
                    | grid A.! (i, j) == splitterChar ->
                        sum
                            [ countTimelines (i, j - 1)
                            , countTimelines (i, j + 1)
                            ]
                    | otherwise -> countTimelines (i + 1, j)

memoize :: (Ix a) => (a, a) -> (a -> b) -> (a -> b)
memoize arrBounds f =
    (cached A.!)
  where
    cached =
        A.listArray
            arrBounds
            (f <$> range arrBounds)
