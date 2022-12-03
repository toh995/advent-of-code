{-# OPTIONS_GHC -Wall #-}

module Day03 where

import Data.Char
import Data.List

type Line = [Char]

fileName :: String
fileName = "data.txt"

main :: IO ()
main = do
  inputStr <- readFile fileName
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

-- Break the given list into sublists of equal size
-- eg: chunksOf [1,2,3,4,5,6,7,8,9] == [[1,2,3],[4,5,6],[7,8,9]]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Given two lists, take the unique intersection of elements
-- eg: intersectUniq [1,1,2,2] [2,2,1,1] == [1,2]
intersectUniq :: Eq a => [a] -> [a] -> [a]
intersectUniq xs ys = nub (intersect xs ys)

-- Given a list of lists, find the unique intersection of all of the inner lists
-- eg intersectLists [ [1,1,2,2], [2,2,1,1] ] == [1,2]
intersectLists :: Eq a => [[a]] -> [a]
intersectLists [] = []
intersectLists lists = foldr1 intersectUniq lists
