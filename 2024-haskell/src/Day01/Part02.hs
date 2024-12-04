module Day01.Part02 (part2) where

import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.Hashable
import Data.Maybe

import Day01.Types

part2 :: Column -> Column -> Int
part2 col1 col2 =
  sum scores
 where
  scores = mapMaybe computeScore col1
  counter2 = buildFreqCounter col2

  computeScore :: Int -> Maybe Int
  computeScore item =
    (item *)
      <$> HashMap.lookup item counter2

type FreqCounter a = HashMap a Int

buildFreqCounter :: (Foldable t, Hashable a) => t a -> FreqCounter a
buildFreqCounter = foldr f HashMap.empty
 where
  f a = HashMap.insertWith (+) a 1
