module Day09.Main where

import Data.Either
import Data.Functor
import Data.List.Split (divvy)
import Data.Void
import Safe
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

filePath :: String
filePath = "src/Day09/data.txt"

main :: IO ()
main = do
  sequences <- readSequences

  let part1Answer = sum <$> mapM nextVal sequences
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = sum <$> mapM prevVal sequences
  putStrLn $ "PART 2: " ++ show part2Answer

readSequences :: IO [[Int]]
readSequences =
  readFile filePath
    <&> fromRight [[]]
      . runParser sequencesP ""

nextVal :: (Eq a, Num a) => [a] -> Maybe a
nextVal [] = Nothing
nextVal ns
  | all (== 0) ns = Just 0
  | otherwise = do
      subSeq <- mapM (foldr1May subtract) . divvy 2 1 $ ns
      (+) <$> lastMay ns <*> nextVal subSeq

prevVal :: (Eq a, Num a) => [a] -> Maybe a
prevVal [] = Nothing
prevVal ns
  | all (== 0) ns = Just 0
  | otherwise = do
      subSeq <- mapM (foldr1May subtract) . divvy 2 1 $ ns
      (-) <$> headMay ns <*> prevVal subSeq

-- Parsing logic
type Parser = Parsec Void String

sequencesP :: (Num a) => Parser [[a]]
sequencesP =
  sequenceP `sepEndBy` newline
    <&> filter (not . null)

sequenceP :: (Num a) => Parser [a]
sequenceP = L.signed hspace L.decimal `sepEndBy` hspace
