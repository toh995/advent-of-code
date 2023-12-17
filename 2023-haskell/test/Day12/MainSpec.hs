module Day12.MainSpec (spec) where

import Test.Hspec

import Day12.Main

spec :: Spec
spec = do
  it "countValidArrangements works" $ do
    countValidArrangements "???.###" [1, 1, 3] `shouldBe` 1
    countValidArrangements ".??..??...?##." [1, 1, 3] `shouldBe` 4
    countValidArrangements "?#?#?#?#?#?#?#?" [1, 3, 1, 6] `shouldBe` 1
    countValidArrangements "????.#...#..." [4, 1, 1] `shouldBe` 1
    countValidArrangements "????.######..#####." [1, 6, 5] `shouldBe` 4
    countValidArrangements "?###????????" [3, 2, 1] `shouldBe` 10
    -- Real dataset
    countValidArrangements "?#?.#.??#.?#?#?????" [1, 1, 1, 9] `shouldBe` 1

  it "unravel works" $ do
    unravel (".#", [1]) `shouldBe` (".#?.#?.#?.#?.#", [1, 1, 1, 1, 1])
    unravel ("???.###", [1, 1, 3])
      `shouldBe` ("???.###????.###????.###????.###????.###", [1, 1, 3, 1, 1, 3, 1, 1, 3, 1, 1, 3, 1, 1, 3])
