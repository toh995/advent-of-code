{-# OPTIONS_GHC -Wall #-}

module Day04 where

import Data.List
import Data.Maybe

type Line = String

data Range = Range [Int]
  deriving (Show)

type RangePair = (Range, Range)

fileName :: String
fileName = "data.txt"

----------------------
-- SECTION: Main IO --
----------------------
main :: IO ()
main = do
  inputStr <- readFile fileName
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
  case (split ',' s) of
    ([s1, s2]) -> (,) <$> strToRange s1 <*> strToRange s2
    _ -> Nothing

-- EXAMPLE: strToRange "12-15" == Just (Range [12,13,14,15])
strToRange :: String -> Maybe Range
strToRange s =
  case ints of
    ([start, end]) -> Just $ Range [start .. end]
    _ -> Nothing
  where
    ints = map read . split '-' $ s

-- EXAMPLE: split '|' "a1|b1|c1" == ["a1", "b1", "c1"]
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
