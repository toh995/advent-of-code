module Day13.Part2 where

import Control.Category ((>>>))
import Data.Function ((&))
import qualified Data.Text as T
import Data.Tuple (swap)
import Text.Read (readMaybe)

part2 :: String -> Integer
part2 =
    parse
        >>> chineseRemainder
        >>> uncurry mod

parse :: String -> [(A, M)]
parse s =
    case lines s of
        [_, line] ->
            line
                & splitOn ","
                & parseEquations
        _ -> error "expected two lines in the input"

parseEquations :: [String] -> [(A, M)]
parseEquations items =
    go items 0
  where
    go [] _ = []
    go (x : xs) i =
        case readMaybe x of
            Just m -> ((-i) `mod` m, m) : go xs (i + 1)
            Nothing -> go xs (i + 1)

splitOn :: String -> String -> [String]
splitOn delim =
    T.pack
        >>> T.splitOn (T.pack delim)
        >>> map (T.unpack)

type A = Integer
type M = Integer
chineseRemainder :: [(A, M)] -> (A, M)
chineseRemainder =
    foldr1 f
  where
    f (a1, m1) (a2, m2) =
        (a, m1 * m2)
      where
        (n1, n2) = extendedEuclid m1 m2
        a = ((a1 * n2 * m2) + (a2 * n1 * m1)) `mod` (m1 * m2)

extendedEuclid :: Integer -> Integer -> (Integer, Integer)
extendedEuclid a b
    | a < 0 || b < 0 = error $ "does not work for negative numbers. got numbers " ++ show a ++ " and " ++ show b
    | a <= b = swap $ extendedEuclid b a
    | b == 0 = (1, 0)
    | otherwise =
        let (x1, y1) = extendedEuclid b (a `mod` b)
            x = y1
            y = x1 - (y1 * (a `div` b))
         in (x, y)
