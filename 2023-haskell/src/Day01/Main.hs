module Day01.Main where

import Day01.Part01
import Day01.Part02

filePath :: String
filePath = "src/Day01/data.txt"

main :: IO ()
main = do
  ls <- lines <$> readFile filePath

  let part1Answer = part1 ls
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 ls
  putStrLn $ "PART 2: " ++ show part2Answer
