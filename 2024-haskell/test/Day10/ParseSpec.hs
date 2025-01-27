module Day10.ParseSpec (spec) where

import Data.HashMap.Lazy qualified as HashMap
import Test.Hspec

import Day10.Matrix
import Day10.Parse

spec :: Spec
spec = do
  describe "parseMatrix" $ do
    it "works" $ do
      let inputStr =
            "0123\n"
              ++ "1234\n"
              ++ "8765\n"
              ++ "9876\n"
      let expected =
            Matrix $
              HashMap.fromList
                [ ((0, 0), 0 :: Int)
                , ((0, 1), 1 :: Int)
                , ((0, 2), 2 :: Int)
                , ((0, 3), 3 :: Int)
                , ((1, 0), 1 :: Int)
                , ((1, 1), 2 :: Int)
                , ((1, 2), 3 :: Int)
                , ((1, 3), 4 :: Int)
                , ((2, 0), 8 :: Int)
                , ((2, 1), 7 :: Int)
                , ((2, 2), 6 :: Int)
                , ((2, 3), 5 :: Int)
                , ((3, 0), 9 :: Int)
                , ((3, 1), 8 :: Int)
                , ((3, 2), 7 :: Int)
                , ((3, 3), 6 :: Int)
                ]
      parseMatrix inputStr
        `shouldBe` Right expected
