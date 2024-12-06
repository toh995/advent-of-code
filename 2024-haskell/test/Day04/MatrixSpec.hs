module Day04.MatrixSpec (spec) where

import Data.HashMap.Lazy qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List
import Test.Hspec

import Day04.Matrix
import Day04.Matrix qualified as Matrix

spec :: Spec
spec = do
  describe "fromRows" $ do
    it "parses a matrix properly" $ do
      let rows =
            [ "abc"
            , "def"
            ]
      Matrix.fromRows rows
        `shouldBe` Matrix
          { iMax = 1
          , jMax = 2
          , hm =
              HashMap.fromList
                [ ((0, 0), 'a')
                , ((0, 1), 'b')
                , ((0, 2), 'c')
                , ((1, 0), 'd')
                , ((1, 1), 'e')
                , ((1, 2), 'f')
                ]
          }

  describe "diags" $ do
    it "computes properly" $ do
      let rows =
            [ "abc"
            , "def"
            , "ghi"
            ]
      let expected = sort ["aei", "bf", "c", "dh", "g", "gec", "hf", "i", "db", "a"]
      let actual =
            sort
              . Matrix.diags
              . Matrix.fromRows
              $ rows
      actual `shouldBe` expected
