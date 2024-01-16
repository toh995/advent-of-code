module Day20.MainSpec (spec) where

import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.List
import Test.Hspec

import Day20.Main

graph :: Graph
graph =
  Graph $
    HashMap.fromList
      [ ("broadcaster", ["a"])
      , ("a", ["inv", "con"])
      , ("inv", ["b"])
      , ("b", ["con"])
      , ("con", ["output"])
      ]

spec :: Spec
spec = do
  it "getInputLabs works" $ do
    let actual = getInputLabs "con" graph
    let expected = ["a", "b"]
    sort actual `shouldBe` sort expected

  it "countPulses1000 works" $ do
    let (g1, mods1) =
          parseAll $
            "broadcaster -> a, b, c\n"
              ++ "%a -> b\n"
              ++ "%b -> c\n"
              ++ "%c -> inv\n"
              ++ "&inv -> a\n"
    countPulses1000 g1 mods1 `shouldBe` Counter{high = 4000, low = 8000}

    let (g2, mods2) =
          parseAll $
            "broadcaster -> a\n"
              ++ "%a -> inv, con\n"
              ++ "&inv -> b\n"
              ++ "%b -> con\n"
              ++ "&con -> output\n"
    countPulses1000 g2 mods2 `shouldBe` Counter{high = 2750, low = 4250}
