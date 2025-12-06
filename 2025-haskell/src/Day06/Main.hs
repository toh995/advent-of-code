module Day06.Main where

import Control.Category ((>>>))
import Data.Char (isDigit)
import Data.Function ((&))
import Data.List (transpose)

data Operator = Add | Mul

filePath :: FilePath
filePath = "src/Day06/data.txt"

main :: IO ()
main = do
    inputStr <- readFile filePath
    let part1Answer = parse1 inputStr & uncurry solve
    putStrLn $ "PART1: " ++ show part1Answer
    let part2Answer = parse2 inputStr & uncurry solve
    putStrLn $ "PART2: " ++ show part2Answer

solve :: [[Int]] -> [Operator] -> Int
solve numss operators =
    sum $
        zipWith eval operators numss

eval :: Operator -> [Int] -> Int
eval Add = sum
eval Mul = product

parse1 :: String -> ([[Int]], [Operator])
parse1 s = (numss, operators)
  where
    numss =
        init rowsRaw
            & map (map read)
            & transpose
    operators =
        last rowsRaw
            & map parseOperator
    rowsRaw =
        lines s
            & map words

parse2 :: String -> ([[Int]], [Operator])
parse2 s = (columns, operators)
  where
    columns =
        init rowsRaw
            & parseColumnsRaw2
            & map parseColumn2
    operators =
        last rowsRaw
            & words
            & map parseOperator
    rowsRaw = lines s

parseColumnsRaw2 :: [String] -> [[String]]
parseColumnsRaw2 rows
    | colWidth == 0 = []
    | otherwise = firstCol : parseColumnsRaw2 rest
  where
    firstCol = map (take colWidth) rows
    rest = map (drop (colWidth + 1)) rows
    colWidth =
        maximum $
            map (length . takeWhile isDigit) rows

parseColumn2 :: [String] -> [Int]
parseColumn2 = transpose >>> map read

parseOperator :: String -> Operator
parseOperator "+" = Add
parseOperator "*" = Mul
parseOperator s = error $ "parse error: invalid operator string " ++ s
