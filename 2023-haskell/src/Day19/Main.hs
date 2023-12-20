module Day19.Main where

import Day19.Part01 qualified
import Day19.Part02 qualified

main :: IO ()
main =
  Day19.Part01.main
    >> Day19.Part02.main
