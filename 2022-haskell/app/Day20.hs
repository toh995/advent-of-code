module Day20 where

import Data.List
import Data.Maybe
import GHC.Utils.Misc

type Index = Int

filePath :: String
filePath = "data/Day20.txt"

main :: IO ()
main = do
  nums <- map read . lines <$> readFile filePath

  let part1Answer = part1 nums
  putStrLn $ "PART 1: " ++ show part1Answer
  
  let part2Answer = part2 nums
  putStrLn $ "PART 2: " ++ show part2Answer

part1 :: [Int] -> Int
part1 nums =
  let mixedIndices = mixList nums [0 .. (length nums) - 1]
      mixedNums = map (\i -> nums !! i) mixedIndices
      zeroIdx = fromJust . findIndex (== 0) $ mixedNums
      first = mixedNums !! ((zeroIdx + 1000) `mod` (length nums))
      second = mixedNums !! ((zeroIdx + 2000) `mod` (length nums))
      third = mixedNums !! ((zeroIdx + 3000) `mod` (length nums))
   in sum [first, second, third]

part2 :: [Int] -> Int
part2 nums =
  let nums' = map (* 811589153) nums
      mixedIndices = nTimes 10 (mixList nums') [0 .. (length nums') - 1]
      mixednums' = map (\i -> nums' !! i) mixedIndices
      zeroIdx = fromJust . findIndex (== 0) $ mixednums'
      first = mixednums' !! ((zeroIdx + 1000) `mod` (length nums'))
      second = mixednums' !! ((zeroIdx + 2000) `mod` (length nums'))
      third = mixednums' !! ((zeroIdx + 3000) `mod` (length nums'))
   in sum [first, second, third]

mixList :: [Int] -> [Index] -> [Index]
mixList nums currIndices =
  foldl' f currIndices (zip [0 .. (length nums) - 1] nums)
  where
    f indices (index, shiftVal) =
      let indexIdx = fromJust . findIndex (== index) $ indices
          marker =
            if indexIdx == (length indices) - 1
              then 0
              else indexIdx
          withoutIndex = delete index indices
          markerAfterShift = (marker + shiftVal) `mod` (length withoutIndex)
          (left, right) = splitAt markerAfterShift withoutIndex
       in left ++ [index] ++ right
