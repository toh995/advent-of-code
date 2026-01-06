module Day11.Main where

import Data.IntMap qualified as IM

main :: IO ()
main = do
    let stoneIds = [20, 82084, 1650, 3, 346355, 363, 7975858, 0]
    putStrLn ("PART1: " ++ show (countStones 25 stoneIds))
    putStrLn ("PART2: " ++ show (countStones 75 stoneIds))

type StoneId = Int

countStones :: Int -> [StoneId] -> Int
countStones numBlinks startStoneIds =
    sum $
        IM.elems finalCounts
  where
    finalCounts = applyN numBlinks step startCounts
    startCounts = IM.fromListWith (+) [(stoneId, 1) | stoneId <- startStoneIds]
    step counts =
        IM.fromListWith
            (+)
            [ (nextStoneId, count)
            | (currStoneId, count) <- IM.assocs counts
            , nextStoneId <- blink currStoneId
            ]

applyN :: Int -> (a -> a) -> a -> a
applyN n f a
    | n < 0 = error $ "cannot apply f " ++ show n ++ " many times"
    | n == 0 = a
    | otherwise = applyN (n - 1) f (f a)

blink :: StoneId -> [StoneId]
blink stoneId
    | stoneId == 0 = [1]
    | Just (leftStr, rightStr) <- splitHalf (show stoneId) = [read leftStr, read rightStr]
    | otherwise = [stoneId * 2024]

splitHalf :: [a] -> Maybe ([a], [a])
splitHalf xs
    | even xLen = Just $ splitAt (xLen `div` 2) xs
    | otherwise = Nothing
  where
    xLen = length xs
