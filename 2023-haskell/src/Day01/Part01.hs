module Day01.Part01 where

import Data.Char
import Data.Maybe
import Safe
import Text.Read

import Util.Types

part1 :: [Line] -> Int
part1 = sum . mapMaybe extractInt

extractInt :: Line -> Maybe Int
extractInt l = do
  let digitChars = filter isDigit l
  first <- headMay digitChars
  second <- lastMay digitChars
  readMaybe [first, second]
