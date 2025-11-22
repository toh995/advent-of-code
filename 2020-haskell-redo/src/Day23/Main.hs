{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Day23.Main where

import Data.Char (digitToInt)
import Data.Function ((&))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

filePath :: String
filePath = "src/Day23/data.txt"

main :: IO ()
main = do
    let cups = digitToInt <$> "853192647"
    let part1Answer = part1 cups
    putStrLn $ "PART1: " ++ part1Answer
    let part2Answer = part2 cups
    putStrLn ("PART2: " ++ show part2Answer)

part1 :: [Int] -> String
part1 cupsInit =
    suffix ++ prefix
        & concatMap show
  where
    finalCups = applyN 100 move1 cupsInit
    (prefix, suffix) = split 1 finalCups

move1 :: [Int] -> [Int]
move1 [] = []
move1 (currCup : cups) =
    let
        (pickedUpCups, rest) = splitAt 3 cups
        findDestCup target
            | target `elem` pickedUpCups = findDestCup $ target - 1
            | target == 0 = findDestCup $ maximum (currCup : cups)
            | otherwise = target
        destCup = findDestCup $ currCup - 1
        (beforeDest, afterDest) = split destCup rest
     in
        beforeDest ++ [destCup] ++ pickedUpCups ++ afterDest ++ [currCup]

split :: (Eq a) => a -> [a] -> ([a], [a])
split x xs = (prefix, suffix)
  where
    (prefix, _, suffix) = splitBy (== x) xs

splitBy :: (a -> Bool) -> [a] -> ([a], a, [a])
splitBy f xs =
    (prefix, head rest, drop 1 rest)
  where
    (prefix, rest) = break f xs

applyN :: Int -> (a -> a) -> a -> a
applyN n f a
    | n < 0 = error ""
    | n == 0 = a
    | otherwise = applyN (n - 1) f (f a)

part2 :: [Int] -> Int
part2 cups =
    iterate (`next` circleFinal) 1
        & drop 1
        & take 2
        & product
  where
    circleInit = fromList $ cups ++ [1 + maximum cups .. 1_000_000]
    (_, circleFinal) = applyN 10_000_000 move2 (head cups, circleInit)

newtype Circle = Circle {im :: IntMap Int}

fromList :: [Int] -> Circle
fromList cups =
    Circle $
        IM.fromList
            ( pairs cups
                ++ [(last cups, head cups)]
            )

pairs :: [a] -> [(a, a)]
pairs (x : y : rest) = (x, y) : pairs (y : rest)
pairs _ = []

next :: Int -> Circle -> Int
next cup Circle{im} = im IM.! cup

setNext :: Int -> Int -> Circle -> Circle
setNext cup nextCup Circle{im} =
    Circle $ IM.insert cup nextCup im

maxCup :: Circle -> Int
maxCup Circle{im} =
    let (ret, _) = IM.findMax im
     in ret

move2 :: (Int, Circle) -> (Int, Circle)
move2 (currCup, circle) = (currCup', circle')
  where
    pickedUpCups =
        iterate (`next` circle) currCup
            & drop 1
            & take 3
    findDestCup target
        | target `elem` pickedUpCups = findDestCup $ target - 1
        | target == 0 = findDestCup (maxCup circle)
        | otherwise = target
    destCup = findDestCup $ currCup - 1
    circle' =
        circle
            & setNext currCup (next (last pickedUpCups) circle)
            & setNext destCup (head pickedUpCups)
            & setNext (last pickedUpCups) (next destCup circle)
    currCup' = next currCup circle'
