module Day18.Part02Spec (spec) where

import Data.Function ((&))
import Test.Hspec
import Text.Parsec (parse)

import Day18.Part02 (Command (..), Direction (..), commandP)

spec :: Spec
spec = do
    it "commandP works" $ do
        let testCases =
                [ ("#70c710", Right Command{direction = R, magnitude = 461937})
                , ("#0dc571", Right Command{direction = D, magnitude = 56407})
                , ("#5713f0", Right Command{direction = R, magnitude = 356671})
                , ("#d2c081", Right Command{direction = D, magnitude = 863240})
                , ("#59c680", Right Command{direction = R, magnitude = 367720})
                , ("#411b91", Right Command{direction = D, magnitude = 266681})
                , ("#8ceee2", Right Command{direction = L, magnitude = 577262})
                , ("#caa173", Right Command{direction = U, magnitude = 829975})
                , ("#1b58a2", Right Command{direction = L, magnitude = 112010})
                , ("#caa171", Right Command{direction = D, magnitude = 829975})
                , ("#7807d2", Right Command{direction = L, magnitude = 491645})
                , ("#a77fa3", Right Command{direction = U, magnitude = 686074})
                , ("#015232", Right Command{direction = L, magnitude = 5411})
                , ("#7a21e3", Right Command{direction = U, magnitude = 500254})
                ]
        let inputs = fst <$> testCases
        let expected = snd <$> testCases
        let actual = inputs & map (\s -> parse commandP s s)
        actual `shouldBe` expected

-- let _ = parse commandP ""
-- pure ()
