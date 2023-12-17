module Day11.Main where

import Day11.Part01 qualified
import Day11.Part02 qualified

main :: IO ()
main =
  Day11.Part01.main
    >> Day11.Part02.main

