module Day11.Part01 where

import Data.Either
import Data.Functor
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type I = Int
type J = Int
type Coord = (I, J)

galaxyChar :: Char
galaxyChar = '#'

emptyChar :: Char
emptyChar = '.'

filePath :: String
filePath = "src/Day11/data.txt"

main :: IO ()
main = do
  rows <- readFile filePath <&> lines
  let expanded = expand rows
  let galaxyCoords = parseGalaxyCoords expanded
  let coordPairs = getAllPairs galaxyCoords

  let part1Answer = sum . map (uncurry manhattanDistance) $ coordPairs
  putStrLn $ "PART 1: " ++ show part1Answer

expand :: [[Char]] -> [[Char]]
expand =
  transpose
    . foldr f []
    . transpose
    . foldr f []
 where
  f xs acc
    | all (== emptyChar) xs = xs : xs : acc
    | otherwise = xs : acc

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (i, j) (i', j') =
  abs (i - i') + abs (j - j')

getAllPairs :: (Ord a) => [a] -> [(a, a)]
getAllPairs as = [(x, y) | x <- as, y <- as, x < y]

-- Getting coordinates
type Parser = Parsec Void String

parseGalaxyCoords :: [String] -> [Coord]
parseGalaxyCoords =
  fromRight []
    . runParser galaxyCoordsP ""
    . unlines

galaxyCoordsP :: Parser [Coord]
galaxyCoordsP =
  delimP
    *> galaxyCoordP `sepEndBy` delimP

delimP :: Parser ()
delimP =
  skipMany $
    char emptyChar
      <|> newline

galaxyCoordP :: Parser Coord
galaxyCoordP = do
  _ <- char galaxyChar
  pos <- getSourcePos
  let i = subtract 1 . unPos . sourceLine $ pos
  let j = subtract 2 . unPos . sourceColumn $ pos
  pure (i, j)
