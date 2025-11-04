module Day10.Main where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List (sort)

filePath :: String
filePath = "src/Day10/data.txt"

main :: IO ()
main = do
    nums <- readFile filePath <&> (lines >>> map read)
    let part1Answer = part1 nums
    putStrLn ("PART 1: " ++ show part1Answer)
    let part2Answer = part2 nums
    putStrLn ("PART 2: " ++ show part2Answer)

part1 :: [Int] -> Int
part1 nums =
    length (filter (== 1) diffs)
        * length (filter (== 3) diffs)
  where
    extraNum = maximum nums + 3
    diffs =
        sort (0 : extraNum : nums)
            & adjacentPairs
            & map (uncurry (-))
            & map abs

part2 :: [Int] -> Int
part2 nums =
    countMemo IM.! 0
  where
    target = maximum nums + 3
    allNums = (0 : target : nums)
    numsSet = IS.fromList allNums
    countMemo = IM.fromList [(num, count num) | num <- allNums]
    count num
        | num == target = 1
        | otherwise =
            sum
                [ countMemo IM.! next
                | next <- (num +) <$> [1, 2, 3]
                , next `IS.member` numsSet
                ]

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs (x : y : rest) = [(x, y)] ++ adjacentPairs (y : rest)
adjacentPairs _ = []
