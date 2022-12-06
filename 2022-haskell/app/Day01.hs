module Day01 where

import Data.List
import Data.List.Split

filePath :: String
filePath = "data/Day01.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath

  let parsed = parse inputStr
  let sortedTotals = sortTotals . computeTotals $ parsed

  let part1Answer = head . take 1 $ sortedTotals
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = sum . take 3 $ sortedTotals
  putStrLn $ "PART 2: " ++ show part2Answer

parse :: String -> [[Int]]
parse =
  map (map read)
    . splitWhen (== "")
    . lines

computeTotals :: [[Int]] -> [Int]
computeTotals = map sum

sortTotals :: [Int] -> [Int]
sortTotals = reverse . sort
