module Day05.MainSpec where

import Day05.Main
import Test.Hspec

spec :: Spec
spec = do
    describe "bitsToInt" $ do
        it "works" $ do
            case parseBitString "FBFBBFF" of
                Left errMsg -> expectationFailure errMsg
                Right bitString -> (bitsToInt bitString) `shouldBe` 44
