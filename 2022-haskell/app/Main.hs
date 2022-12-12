module Main where

import Day11.Part01 hiding (main)
import qualified Day11.Part01

import Data.List.Split
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- main :: IO ()
-- main = Day11.Part01.main

main = do
  inputStr <- readFile filePath
  let monkeys = parseMonkeys . splitOn [""] . lines $ inputStr

  -- print $ getNextItem <$> IntMap.lookup 0 monkeys <*> pure 79
  -- print $ testNextItem <$> IntMap.lookup 0 monkeys <*> pure 79
  -- print $ testNextItem <$> IntMap.lookup 0 monkeys <*> pure 79
  -- print $ trueMonkey <$> IntMap.lookup 0 monkeys
  -- print $ falseMonkey <$> IntMap.lookup 0 monkeys
  -- print $ getNextKey <$> IntMap.lookup 0 monkeys <*> pure 79

  print monkeys

  print $ doRounds 1 monkeys
  print $ doRounds 20 monkeys
  print $ doRounds 1000 monkeys
