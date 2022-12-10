module Day10 where

import Data.List
import Data.List.Split
import Data.Maybe
import Text.Read

type Line = String
type Instruction = (Int -> Int)
type Value = Int

----------------------
-- SECTION: Main IO --
----------------------
filePath :: String
filePath = "data/Day10.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath
  let instructions = parseLines . lines $ inputStr
  let values = computeValues instructions

  let part1Answer = part1 values
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 values
  putStrLn $ "PART 2:"
  putStrLn part2Answer

----------------------
-- SECTION: Parsing --
----------------------
parseLines :: [Line] -> [Instruction]
parseLines = concat . mapMaybe parseLine

parseLine :: Line -> Maybe [Instruction]
parseLine l =
  case (words l) of
       ["noop"] -> Just [id]
       ["addx", strNum] -> (:) id <$> (singleton . (+) <$> readMaybe strNum)
       _ -> Nothing

computeValues :: [Instruction] -> [Value]
computeValues = scanl' (flip ($)) 1

---------------------
-- SECTION: Part 1 --
---------------------
part1 :: [Value] -> Int
part1 values =
  sum
  . map (computeSignalStrength values)
  $ [20, 60, 100, 140, 180, 220]

computeSignalStrength :: [Value] -> Int -> Int
computeSignalStrength values i = i * values !! (i-1)

---------------------
-- SECTION: Part 2 --
---------------------
part2 :: [Value] -> String
part2 values =
  buildDisplayString
  . map computePixelChar
  $ zip values (cycle [0..39])

computePixelChar :: (Value, Int) -> Char
computePixelChar (v, idx)
  | abs (v - idx) <= 1 = '#'
  | otherwise          = '.'

buildDisplayString :: [Char] -> String
buildDisplayString = unlines . chunksOf 40
