module Day01.Part01 (part1) where

import Data.List

import Day01.Types

part1 :: Column -> Column -> Int
part1 col1 col2 =
  let
    sorted1 = sort col1
    sorted2 = sort col2
    distances = zipWith distance sorted1 sorted2
   in
    sum distances

distance :: Int -> Int -> Int
distance x y = abs $ x - y
