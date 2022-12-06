module Day06 where

import Data.List
import Data.List.Split

filePath :: String
filePath = "data/Day06.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath

  let part1Answer = part1 inputStr
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 inputStr
  putStrLn $ "PART 2: " ++ show part2Answer

part1 :: String -> Maybe Int
part1 = compute 4

part2 :: String -> Maybe Int
part2 = compute 14

compute :: Int -> String -> Maybe Int
compute len s =
  (+ len) <$> findIndex allUnique substrings
  where
    substrings = divvy len 1 s

allUnique :: Eq a => [a] -> Bool
allUnique = (==) <$> length <*> length . nub
