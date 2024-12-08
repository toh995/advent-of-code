module Day08.MainSpec (spec) where

import Data.HashMap.Lazy qualified as HashMap
import Test.Hspec

import Day08.Main

inputRows :: [String]
inputRows =
  [ "............"
  , "........0..."
  , ".....0......"
  , ".......0...."
  , "....0......."
  , "......A....."
  , "............"
  , "............"
  , "........A..."
  , ".........A.."
  , "............"
  , "............"
  ]

spec :: Spec
spec = do
  describe "parseCoordMap" $ do
    it "works" $ do
      parseCoordMap inputRows
        `shouldBe` HashMap.fromList
          [ ('0', reverse [(1, 8), (2, 5), (3, 7), (4, 4)])
          , ('A', reverse [(5, 6), (8, 8), (9, 9)])
          ]
