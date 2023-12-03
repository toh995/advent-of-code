module Day02.Main where

import Data.Either
import Data.Functor
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Color = Blue | Green | Red
  deriving (Eq, Show)

data Game = Game
  { gameId :: Int
  , colorCounts :: [(Int, Color)]
  }
  deriving (Show)

main :: IO ()
main = do
  games <- readGames
  let part1Answer = part1 games
  putStrLn $ "PART 1: " ++ show part1Answer
  let part2Answer = part2 games
  putStrLn $ "PART 2: " ++ show part2Answer

readGames :: IO [Game]
readGames =
  readFile "src/Day02/data.txt"
    >>= pure
      . rights
      . map (runParser gameP "")
      . lines

-- Part 1
part1 :: [Game] -> Int
part1 =
  sum
    . map gameId
    . filter isValidGame

isValidGame :: Game -> Bool
isValidGame (Game{colorCounts}) = all isValidColorCount colorCounts

isValidColorCount :: (Int, Color) -> Bool
isValidColorCount (n, Blue) = n <= 14
isValidColorCount (n, Green) = n <= 13
isValidColorCount (n, Red) = n <= 12

-- Part 2
part2 :: [Game] -> Int
part2 = sum . map gamePower

gamePower :: Game -> Int
gamePower g =
  maxBlue * maxGreen * maxRed
 where
  maxBlue = maxCount Blue g
  maxGreen = maxCount Green g
  maxRed = maxCount Red g

maxCount :: Color -> Game -> Int
maxCount color (Game{colorCounts}) =
  maximum
    . map fst
    . filter ((== color) . snd)
    $ colorCounts

-- Parsing Logic
type Parser = Parsec Void String

gameP :: Parser Game
gameP = Game <$> gameIdP <*> colorCountsP

gameIdP :: Parser Int
gameIdP = (string "Game " *> L.decimal <* string ": ")

colorCountsP :: Parser [(Int, Color)]
colorCountsP =
  sepBy
    colorCountP
    (string ", " <|> string "; ")

colorCountP :: Parser (Int, Color)
colorCountP =
  (,)
    <$> (L.decimal <* char ' ')
    <*> colorP

colorP :: Parser Color
colorP =
  (string "blue" $> Blue)
    <|> (string "green" $> Green)
    <|> (string "red" $> Red)
