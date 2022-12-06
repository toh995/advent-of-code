module Day03 where

import Data.Char
import Data.List
import Data.List.Split

type Line = [Char]

filePath :: String
filePath = "data/Day03.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath
  let ls = lines inputStr

  let part1Answer = part1 ls
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 ls
  putStrLn $ "PART 2: " ++ show part2Answer

---------------------
-- SECTION: Part 1 --
---------------------
part1 :: [Line] -> Int
part1 = sum . map linePriority1

linePriority1 :: Line -> Int
linePriority1 =
  sum
    . map charPriority
    . uncurry intersectUniq
    . splitInHalf

---------------------
-- SECTION: Part 2 --
---------------------
part2 :: [Line] -> Int
part2 =
  sum
    . map groupPriority2
    . chunksOf 3

groupPriority2 :: [Line] -> Int
groupPriority2 =
  sum
    . map charPriority
    . intersectLists

--------------------
-- SECTION: Utils --
--------------------
splitInHalf :: [a] -> ([a], [a])
splitInHalf l =
  let halfLen = length l `div` 2
   in splitAt halfLen l

-- Given a char, find the integer priority avalue
-- This fn is a bit messy, but oh well
charPriority :: Char -> Int
charPriority c
  | isLower c = ord c - ord 'a' + 1
  | isUpper c = charPriority (toLower c) + 26
  | otherwise = 0

-- Given two lists, take the unique intersection of elements
-- eg: intersectUniq [1,1,2,2] [2,2,1,1] == [1,2]
intersectUniq :: Eq a => [a] -> [a] -> [a]
intersectUniq xs ys = nub (intersect xs ys)

-- Given a list of lists, find the unique intersection of all of the inner lists
-- eg intersectLists [ [1,1,2,2], [2,2,1,1] ] == [1,2]
intersectLists :: Eq a => [[a]] -> [a]
intersectLists [] = []
intersectLists lists = foldr1 intersectUniq lists
