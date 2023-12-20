module Day19.Part02Spec (spec) where

import Test.Hspec
import Text.Megaparsec

import Day19.Part02

spec :: Spec
spec = do
  it "edgesP works" $ do
    let result = runParser (edgesP []) "" "a<2006:qkq,m>2090:A,rfg"
    let expected =
          Right
            [ Edge{destLabel = "qkq", comps = [Comp A LT 2006]}
            , Edge{destLabel = "A", comps = [Comp M GT 2090, Comp A GT 2005]}
            , Edge{destLabel = "rfg", comps = [Comp M LT 2091, Comp A GT 2005]}
            ]
    result `shouldBe` expected
