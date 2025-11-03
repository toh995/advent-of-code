module Day01.Main where

import Control.Category ((>>>))
import Data.Foldable (find)
import Data.Functor ((<&>))
import qualified Data.IntSet as IS
import Data.Maybe (fromJust)

filePath :: String
filePath = "src/Day01/data.txt"

main :: IO ()
main = do
    nums <- readFile filePath <&> (lines >>> map read)

    let part1Answer = part1 nums
    putStrLn ("PART 1: " ++ show part1Answer)

    let part2Answer = part2 nums
    putStrLn ("PART 2: " ++ show part2Answer)

part1 :: [Int] -> Int
part1 nums =
    x * y
  where
    (x, y) = fromJust $ twoSum 2020 nums

part2 :: [Int] -> Int
part2 nums =
    x * y * z
  where
    (x, y, z) = fromJust $ threeSum 2020 nums

twoSum :: Int -> [Int] -> Maybe (Int, Int)
twoSum target nums = do
    let numSet = IS.fromList nums
    addend1 <- find (\n -> (target - n) `IS.member` numSet) nums
    let addend2 = target - addend1
    Just (addend1, addend2)

threeSum :: Int -> [Int] -> Maybe (Int, Int, Int)
threeSum target (a : as) =
    case twoSum (target - a) as of
        Just (x, y) -> Just (a, x, y)
        Nothing -> threeSum target as
threeSum _ [] = Nothing
