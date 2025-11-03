{-# LANGUAGE NamedFieldPuns #-}

module Day05.Main where

import Control.Category ((>>>))
import Data.Foldable (find)
import Data.Function ((&))
import Data.List (sort)
import Data.Maybe (fromJust)

filePath :: String
filePath = "src/Day05/data.txt"

main :: IO ()
main = do
    inputStr <- readFile filePath
    let bitStrings =
            case inputStr & lines & mapM parseBitString of
                Right val -> val
                Left errMsg -> error errMsg
    let part1Answer = part1 bitStrings
    putStrLn ("PART 1: " ++ show part1Answer)

    let part2Answer = part2 bitStrings
    putStrLn ("PART 2: " ++ show part2Answer)

part1 :: [[Bit]] -> Int
part1 =
    map seatFromBits
        >>> map seatId
        >>> maximum

part2 :: [[Bit]] -> Int
part2 bitStrings =
    seatIds
        & sort
        & pairs
        & find (\(id1, id2) -> id1 + 2 == id2)
        & fromJust
        & (\(_, id2) -> id2 - 1)
  where
    seatIds =
        bitStrings
            & map seatFromBits
            & map seatId

pairs :: [a] -> [(a, a)]
pairs (a : b : as) = (a, b) : pairs (b : as)
pairs _ = []

data Bit = Lower | Upper
    deriving (Show)

data Seat = Seat
    { rowIdx :: Int
    , colIdx :: Int
    }

seatId :: Seat -> Int
seatId Seat{rowIdx, colIdx} = (rowIdx * 8) + colIdx

seatFromBits :: [Bit] -> Seat
seatFromBits bits =
    Seat
        { rowIdx = bitsToInt rowBits
        , colIdx = bitsToInt colBits
        }
  where
    (rowBits, colBits) = splitAt 7 bits

bitsToInt :: [Bit] -> Int
bitsToInt bits =
    go startRange bits
  where
    mid (lower, upper) = (fromIntegral lower + fromIntegral upper :: Double) / 2
    startRange = (0, (2 ^ (length bits)) - 1)

    go (lower, upper) []
        | lower == upper = lower
        | otherwise = error $ "FATAL BAD ALGORITHM IN bitsToInt, for input " ++ show bits
    go range@(_, upper) (Upper : bs) = go (ceiling (mid range), upper) bs
    go range@(lower, _) (Lower : bs) = go (lower, floor (mid range)) bs

parseBitString :: String -> Either String [Bit]
parseBitString = mapM parseBit

parseBit :: Char -> Either String Bit
parseBit 'F' = Right Lower
parseBit 'B' = Right Upper
parseBit 'L' = Right Lower
parseBit 'R' = Right Upper
parseBit c = Left $ "invalid bit char " ++ [c]
