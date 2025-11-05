module Day12.Main where

import Day12.Part1 (part1)
import Day12.Part2 (part2)

filePath :: String
filePath = "src/Day12/data.txt"

main :: IO ()
main = do
    inputStr <- readFile filePath

    let part1Answer = part1 inputStr
    putStrLn ("PART 1: " ++ show part1Answer)
    let part2Answer = part2 inputStr
    putStrLn ("PART 2: " ++ show part2Answer)
    pure ()
