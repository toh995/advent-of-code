{-# LANGUAGE LambdaCase #-}

module Day03.Main where

import Control.Category ((>>>))
import Data.Char (digitToInt)
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

type Digit = Int

filePath :: FilePath
filePath = "src/Day03/data.txt"

main :: IO ()
main = do
    digitss <-
        readFile filePath
            <&> ( lines
                    >>> map (map digitToInt)
                )
    let part1Answer = solve 2 digitss
    putStrLn $ "PART1: " ++ show part1Answer
    let part2Answer = solve 12 digitss
    putStrLn $ "PART2: " ++ show part2Answer

solve :: Int -> [[Digit]] -> Int
solve turnOnAmt =
    map (fromJust . maxJoltage turnOnAmt)
        >>> sum

maxJoltage :: Int -> [Digit] -> Maybe Int
maxJoltage n digits =
    digitsToInt
        <$> answerDigits n (Seq.fromList digits)
  where
    answerDigits k ds
        | k < 0 = Nothing
        | k == 0 = Just []
        | otherwise =
            ds
                & Seq.tails
                & Seq.takeWhileL ((>= k) . Seq.length)
                & maximumByL (compare `on` seqHead)
                & ( \case
                        Empty -> Nothing
                        (digit :<| rest) -> (digit :) <$> answerDigits (k - 1) rest
                  )

digitsToInt :: [Digit] -> Int
digitsToInt = foldl (\acc d -> (acc * 10) + d) 0

seqHead :: Seq a -> a
seqHead = (`Seq.index` 0)

maximumByL :: (Foldable t) => (a -> a -> Ordering) -> t a -> a
maximumByL cmp =
    foldl1 f
  where
    f acc x =
        case cmp acc x of
            GT -> acc
            EQ -> acc
            LT -> x
