module Day18.Main where

import Day18.Part01 qualified
import Day18.Part02 qualified

main :: IO ()
main =
    Day18.Part01.main
        >> Day18.Part02.main
