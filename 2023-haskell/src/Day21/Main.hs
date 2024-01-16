module Day21.Main where

import Day21.Part01 qualified
import Day21.Part02 qualified
import Day21.Part022 qualified

main :: IO ()
main =
  Day21.Part01.main
    >> Day21.Part022.main
