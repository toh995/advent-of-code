module Main where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Day09 hiding (main)
import qualified Day09

main :: IO ()
-- main = print "hello"

-- main = Day09.main

main = do
  inputStr <- readFile filePath
  let directions = parseLines . lines $ inputStr

  mapM_ print $ executeDirections 10 directions
  print $ Set.size . Set.fromList . getTails $ executeDirections 10 directions
