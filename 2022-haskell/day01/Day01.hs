{-# OPTIONS_GHC -Wall #-}

module Day01 where

import Data.List
import Data.Maybe

fileName :: String
fileName = "data.txt"

main :: IO ()
main = do
  inputStr <- readFile fileName
  let parsed = parse inputStr
  let sortedTotals = sortTotals . computeTotals $ parsed

  let part1Answer = head . take 1 $ sortedTotals
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = sum . take 3 $ sortedTotals
  putStrLn $ "PART 2: " ++ show part2Answer

parse :: String -> [[Int]]
parse =
  map (map read)
    . split ""
    . lines

computeTotals :: [[Int]] -> [Int]
computeTotals = map sum

sortTotals :: [Int] -> [Int]
sortTotals = reverse . sort

-- EXAMPLE
-- split "" ["1","","2","","3","4"] == [["1"], ["2"], ["3","4"]]
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split delim xs =
  let sublist = takeWhile (/= delim) xs
      rest =
        fromMaybe []
          . stripPrefix [delim]
          . dropWhile (/= delim)
          $ xs
   in sublist : split delim rest
