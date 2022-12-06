module Day04 where

import Data.List
import Data.List.Split
import Data.Maybe

type Line = String

data Range = Range [Int]
  deriving (Show)

type RangePair = (Range, Range)

filePath :: String
filePath = "data/Day04.txt"

----------------------
-- SECTION: Main IO --
----------------------
main :: IO ()
main = do
  inputStr <- readFile filePath
  let rangePairs = parse inputStr

  let part1Answer = part1 rangePairs
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 rangePairs
  putStrLn $ "PART 2: " ++ show part2Answer

----------------------
-- SECTION: Parsing --
----------------------
parse :: String -> [RangePair]
parse = mapMaybe parseLine . lines

parseLine :: Line -> Maybe RangePair
parseLine s =
  case (splitWhen (== ',') s) of
    ([s1, s2]) -> (,) <$> strToRange s1 <*> strToRange s2
    _ -> Nothing

-- EXAMPLE: strToRange "12-15" == Just (Range [12,13,14,15])
strToRange :: String -> Maybe Range
strToRange s =
  case ints of
    ([start, end]) -> Just $ Range [start .. end]
    _ -> Nothing
  where
    ints = map read . splitWhen (== '-') $ s

-------------------------
-- SECTION: main logic --
-------------------------
part1 :: [RangePair] -> Int
part1 = length . filter hasFullOverlap

part2 :: [RangePair] -> Int
part2 = length . filter hasSomeOverlap

hasFullOverlap :: RangePair -> Bool
hasFullOverlap (Range ns, Range ms) =
  isInfixOf ns ms || flip isInfixOf ns ms

hasSomeOverlap :: RangePair -> Bool
hasSomeOverlap (Range ns, Range ms) =
  not . null . uncurry intersect $ (ns, ms)
