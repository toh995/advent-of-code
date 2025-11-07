module Day15.Main where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

main :: IO ()
main = do
    let startNums = [9, 6, 0, 10, 18, 2, 1]
    putStrLn $ "PART1: " ++ show (solve 2020 startNums)
    putStrLn $ "PART2: " ++ show (solve 30000000 startNums)

solve :: Int -> [Int] -> Int
solve targetTurnNum startNums = finalVal
  where
    seenInit =
        IM.fromList $
            zip (init startNums) [1 ..]
    startState = (last startNums, length startNums, seenInit)
    (finalVal, _, _) =
        applyN
            (targetTurnNum - length startNums)
            doTurn
            startState

-- seen is a map from value to last seen turn number
doTurn :: (Int, Int, IntMap Int) -> (Int, Int, IntMap Int)
doTurn (val, turnNum, seen) =
    case seen IM.!? val of
        Just prevTurnNum -> (turnNum - prevTurnNum, turnNum + 1, seen')
        Nothing -> (0, turnNum + 1, seen')
  where
    seen' = IM.insert val turnNum seen

-- apply the function n times
applyN :: Int -> (a -> a) -> a -> a
applyN n f x
    | n <= 0 = x
    | otherwise = applyN (n - 1) f (f x)
