module Day25.Main where

import Data.Function ((&))
import Data.List (elemIndex)
import Data.Maybe (fromJust)

filePath :: String
filePath = "src/Day25/data.txt"

main :: IO ()
main = do
    let cardKey = 14205034
    let doorKey = 18047856
    let part1Answer = part1 cardKey doorKey
    putStrLn $ "PART1: " ++ show part1Answer

part1 :: Int -> Int -> Int
part1 cardKey doorKey =
    encryptionKey
  where
    encryptionKey = applyN cardLoopSize (step doorKey) 1
    cardLoopSize = loopSize 7 cardKey

loopSize :: Int -> Int -> Int
loopSize subjectNum target =
    iterate (step subjectNum) 1
        & elemIndex target
        & fromJust

step :: Int -> Int -> Int
step subjectNum value = (value * subjectNum) `mod` 20201227

applyN :: Int -> (a -> a) -> a -> a
applyN n f = (!! n) . iterate f
