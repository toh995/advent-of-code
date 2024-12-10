module Day09.ParseSpec (spec) where

import Test.Hspec

import Day09.Parse
import Day09.Types

spec :: Spec
spec = do
  describe "parseBlocks" $ do
    it "works" $ do
      parseBlocks "12345"
        `shouldBe` Right
          [ File{id = 0, size = 1}
          , FreeSpace{size = 2}
          , File{id = 1, size = 3}
          , FreeSpace{size = 4}
          , File{id = 2, size = 5}
          ]
      parseBlocks "1234"
        `shouldBe` Right
          [ File{id = 0, size = 1}
          , FreeSpace{size = 2}
          , File{id = 1, size = 3}
          , FreeSpace{size = 4}
          ]
      parseBlocks "2333133121414131402"
        `shouldBe` Right
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
