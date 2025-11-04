{-# LANGUAGE MultiWayIf #-}

module Day09.Main where

import Control.Category ((>>>))
import Data.Foldable (toList)
import Data.Functor ((<&>))
import qualified Data.IntSet as IS
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

filePath :: String
filePath = "src/Day09/data.txt"

main :: IO ()
main = do
    nums <- readFile filePath <&> (lines >>> map read)

    let part1Answer = part1 nums
    putStrLn ("PART 1: " ++ show part1Answer)

    let part2Answer = part2 nums
    putStrLn ("PART 2: " ++ show part2Answer)

part1 :: [Int] -> Int
part1 nums =
    go prefixInit targetsInit
  where
    prefixInit = Seq.fromList $ take 25 nums
    targetsInit = drop 25 nums
    go _ [] = error "could not find element"
    go Empty _ = error "bad!! we shouldn't have an empty prefix window!!"
    go pss@(_ :<| ps) (target : xs) =
        if
            | hasTwoSum target (toList pss) -> go (ps :|> target) xs
            | otherwise -> target

hasTwoSum :: Int -> [Int] -> Bool
hasTwoSum target nums =
    any
        (\num -> (target - num) `IS.member` numSet)
        nums
  where
    numSet = IS.fromList nums

part2 :: [Int] -> Int
part2 nums =
    go Seq.empty nums 0
  where
    target = part1 nums
    go window suffix currSum
        | currSum < target =
            case suffix of
                (n : suffix') -> go (window :|> n) suffix' (currSum + n)
                [] -> error "asdfasd"
        | currSum > target =
            case window of
                (n :<| window') -> go window' suffix (currSum - n)
                Empty -> error "asdfasdf"
        | otherwise = minimum window + maximum window
