module Day07.Main where

import Day07.Part01 qualified as Part01
import Day07.Part02 qualified as Part02

main :: IO ()
main = Part01.main >> Part02.main
