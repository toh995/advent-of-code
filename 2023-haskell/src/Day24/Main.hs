module Day24.Main where

import Day24.Part01 (part1)
import Day24.Part02 (part2)

filePath :: FilePath
filePath = "src/Day24/data.txt"

main :: IO ()
main = do
    inputStr <- readFile filePath
    let part1Answer = part1 inputStr
    putStrLn $ "PART1: " ++ show part1Answer
    let part2Answer = part2 inputStr
    putStrLn $ "PART2: " ++ show part2Answer
