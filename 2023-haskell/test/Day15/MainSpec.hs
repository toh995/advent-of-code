module Day15.MainSpec (spec) where

import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as IntMap
import Test.Hspec

import Day15.Main

commands :: [Command]
commands =
  [ Upsert (Lens{label = "rn", focalLength = 1})
  , Delete "cm"
  , Upsert (Lens{label = "qp", focalLength = 3})
  , Upsert (Lens{label = "cm", focalLength = 2})
  , Delete "qp"
  , Upsert (Lens{label = "pc", focalLength = 4})
  , Upsert (Lens{label = "ot", focalLength = 9})
  , Upsert (Lens{label = "ab", focalLength = 5})
  , Delete "pc"
  , Upsert (Lens{label = "pc", focalLength = 6})
  , Upsert (Lens{label = "ot", focalLength = 7})
  ]

spec :: Spec
spec = do
  it "hash works" $ do
    hash "HASH" `shouldBe` 52
    hash "rn=1" `shouldBe` 30
    hash "cm-" `shouldBe` 253
    hash "qp=3" `shouldBe` 97
    hash "cm=2" `shouldBe` 47
    hash "qp-" `shouldBe` 14
    hash "pc=4" `shouldBe` 180
    hash "ot=9" `shouldBe` 9
    hash "ab=5" `shouldBe` 197
    hash "pc-" `shouldBe` 48
    hash "pc=6" `shouldBe` 214
    hash "ot=7" `shouldBe` 231

  it "executeCommands works" $ do
    let im =
          IntMap.filter (/= [])
            . executeCommands
            $ commands
    im
      `shouldBe` IntMap.fromList
        [
          ( 0
          ,
            [ Lens{label = "rn", focalLength = 1}
            , Lens{label = "cm", focalLength = 2}
            ]
          )
        ,
          ( 3
          ,
            [ Lens{label = "ot", focalLength = 7}
            , Lens{label = "ab", focalLength = 5}
            , Lens{label = "pc", focalLength = 6}
            ]
          )
        ]

  it "part2 works" $ do
    part2 commands `shouldBe` 145
