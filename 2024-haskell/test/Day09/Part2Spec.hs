module Day09.Part2Spec (spec) where

import Test.Hspec

import Day09.Part2
import Day09.Types

sampleBlocks :: [Block]
sampleBlocks =
  [ File{size = 2, id = 0}
  , FreeSpace{size = 3}
  , File{size = 3, id = 1}
  , FreeSpace{size = 3}
  , File{size = 1, id = 2}
  , FreeSpace{size = 3}
  , File{size = 3, id = 3}
  , FreeSpace{size = 1}
  , File{size = 2, id = 4}
  , FreeSpace{size = 1}
  , File{size = 4, id = 5}
  , FreeSpace{size = 1}
  , File{size = 4, id = 6}
  , FreeSpace{size = 1}
  , File{size = 3, id = 7}
  , FreeSpace{size = 1}
  , File{size = 4, id = 8}
  , FreeSpace{size = 0}
  , File{size = 2, id = 9}
  ]

spec :: Spec
spec = do
  describe "compress" $ do
    it "works" $ do
      compress sampleBlocks
        `shouldBe` [ File{id = 0, size = 2}
                   , File{id = 9, size = 2}
                   , File{id = 2, size = 1}
                   , FreeSpace{size = 0}
                   , File{id = 1, size = 3}
                   , File{id = 7, size = 3}
                   , FreeSpace{size = 1}
                   , File{id = 4, size = 2}
                   , FreeSpace{size = 1}
                   , File{id = 3, size = 3}
                   , FreeSpace{size = 4}
                   , File{id = 5, size = 4}
                   , FreeSpace{size = 1}
                   , File{id = 6, size = 4}
                   , FreeSpace{size = 5}
                   , File{id = 8, size = 4}
                   , FreeSpace{size = 2}
                   ]
