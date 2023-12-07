module Day05.Main where

import Day05.Part01
import Day05.Part02

filePath :: String
filePath = "src/Day05/data.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath

  let part1Answer = part1 inputStr
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 inputStr
  putStrLn $ "PART 2: " ++ show part2Answer
