module Day18.Part02Spec (spec) where

import Test.Hspec

import Day18.Part02

spec :: Spec
spec = do
  it "hexToInt works" $ do
    hexToInt "70c71" `shouldBe` 461937
    hexToInt "0dc57" `shouldBe` 56407
    hexToInt "5713f" `shouldBe` 356671
    hexToInt "d2c08" `shouldBe` 863240
    hexToInt "59c68" `shouldBe` 367720
    hexToInt "411b9" `shouldBe` 266681
    hexToInt "8ceee" `shouldBe` 577262
    hexToInt "caa17" `shouldBe` 829975
    hexToInt "1b58a" `shouldBe` 112010
    hexToInt "caa17" `shouldBe` 829975
    hexToInt "7807d" `shouldBe` 491645
    hexToInt "a77fa" `shouldBe` 686074
    hexToInt "01523" `shouldBe` 5411
    hexToInt "7a21e" `shouldBe` 500254

  it "parseCommands works" $ do
    let inputStr =
          "R 6 (#70c710)\n"
            ++ "D 5 (#0dc571)\n"
            ++ "L 2 (#5713f0)\n"
            ++ "D 2 (#d2c081)\n"
            ++ "R 2 (#59c680)\n"
            ++ "D 2 (#411b91)\n"
            ++ "L 5 (#8ceee2)\n"
            ++ "U 2 (#caa173)\n"
            ++ "L 1 (#1b58a2)\n"
            ++ "U 2 (#caa171)\n"
            ++ "R 2 (#7807d2)\n"
            ++ "U 3 (#a77fa3)\n"
            ++ "L 2 (#015232)\n"
            ++ "U 2 (#7a21e3)\n"
    let commands = parseCommands inputStr
    commands
      `shouldBe` [ Command{dir = R, magnitude = 461937}
                 , Command{dir = D, magnitude = 56407}
                 , Command{dir = R, magnitude = 356671}
                 , Command{dir = D, magnitude = 863240}
                 , Command{dir = R, magnitude = 367720}
                 , Command{dir = D, magnitude = 266681}
                 , Command{dir = L, magnitude = 577262}
                 , Command{dir = U, magnitude = 829975}
                 , Command{dir = L, magnitude = 112010}
                 , Command{dir = D, magnitude = 829975}
                 , Command{dir = L, magnitude = 491645}
                 , Command{dir = U, magnitude = 686074}
                 , Command{dir = L, magnitude = 5411}
                 , Command{dir = U, magnitude = 500254}
                 ]
