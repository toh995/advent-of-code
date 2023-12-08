module Day08.Part02Spec (spec) where

import Data.HashMap.Lazy qualified as HashMap
import GHC.Arr
import Test.Hspec

import Day08.Part02

dirs :: Array Int Direction
dirs = listArray (0, 2) [L, L, R]

tree :: Tree
tree =
  Tree $
    HashMap.fromList
      [ (Node "AAA", (Node "BBB", Node "BBB"))
      , (Node "BBB", (Node "AAA", Node "ZZZ"))
      , (Node "ZZZ", (Node "ZZZ", Node "ZZZ"))
      ]

-- LLR
--
-- AAA = (BBB, BBB)
-- BBB = (AAA, ZZZ)
-- ZZZ = (ZZZ, ZZZ)

spec :: Spec
spec = do
  -- it "findCycle works" $ do
  --   -- Positive cases
  --   findCycle ([1, 2, 3, 4, 1] :: [Int]) `shouldBe` Just Cycle{cycleLen = 4, offset = 0}
  --   findCycle ([0, 1, 3, 1] :: [Int]) `shouldBe` Just Cycle{cycleLen = 2, offset = 1}
  --   findCycle ([-1, 1, 2, 3, 4, 1, 2, 3, 4] :: [Int]) `shouldBe` Just Cycle{cycleLen = 4, offset = 1}
  --   findCycle ([-1, 0, 1, 2, 3, 4, 1, 2, 3, 4] :: [Int]) `shouldBe` Just Cycle{cycleLen = 4, offset = 2}
  --   findCycle ([-5 .. -1] ++ cycle [100, 99 .. 1] :: [Int]) `shouldBe` Just Cycle{cycleLen = 100, offset = 5}
  --
  --   -- Negative cases
  --   findCycle ([] :: [Int]) `shouldBe` Nothing
  --   findCycle ([1, 2, 3, 4] :: [Int]) `shouldBe` Nothing

  it "nextPos works" $ do
    let actual =
          take 7 $
            iterate
              (nextPos tree dirs)
              Pos{dirIdx = 0, node = Node "AAA"}
    let expected =
          [ Pos{dirIdx = 0, node = Node "AAA"}
          , Pos{dirIdx = 1, node = Node "BBB"}
          , Pos{dirIdx = 2, node = Node "AAA"}
          , Pos{dirIdx = 0, node = Node "BBB"}
          , Pos{dirIdx = 1, node = Node "AAA"}
          , Pos{dirIdx = 2, node = Node "BBB"}
          , Pos{dirIdx = 0, node = Node "ZZZ"}
          ]
    actual `shouldBe` expected

-- it "findCycle combined with nextPos" $ do
--   let positions =
--         iterate
--           (nextPos tree dirs)
--           Pos{dirIdx = 0, node = Node "AAA"}
--   let (Just Cycle{offset, cycleLen}) =
--         findCycle positions
--   print take (offset + cycleLen) $ positions
