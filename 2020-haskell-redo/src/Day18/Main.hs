module Day18.Main where

import Day18.Part1 (part1)
import Day18.Part2 (part2)

filePath :: String
filePath = "src/Day18/data.txt"

main :: IO ()
main = do
    inputStr <- readFile filePath
    let part1Answer = part1 inputStr
    putStrLn ("PART1: " ++ show part1Answer)
    let part2Answer = part2 inputStr
    putStrLn ("PART2: " ++ show part2Answer)
