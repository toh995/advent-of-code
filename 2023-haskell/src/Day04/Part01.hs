module Day04.Part01 where

import Data.Either
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Card = Card
  { winningInts, myInts :: [Int]
  }
  deriving (Show)

computePoints :: Card -> Int
computePoints (Card{winningInts, myInts}) =
  -- If we need more efficiency here, we can change from
  -- list intersection to set intersection
  let numMatches = length $ intersect winningInts myInts
   in if
        | numMatches == 0 -> 0
        | otherwise -> 2 ^ (numMatches - 1)

-- Main logic
part1 :: String -> Int
part1 =
  sum
    . map computePoints
    . parseCards

-- Parsing Logic
parseCards :: String -> [Card]
parseCards =
  fromRight []
    . runParser cardsP ""

type Parser = Parsec Void String

cardsP :: Parser [Card]
cardsP = cardP `sepEndBy` newline

cardP :: Parser Card
cardP = do
  _ <-
    string "Card"
      *> hspace
      *> (L.decimal :: Parser Int)
      *> char ':'
      *> hspace
  winningInts <- L.decimal `sepEndBy` hspace
  _ <- char '|' *> hspace
  myInts <- L.decimal `sepEndBy` hspace
  pure Card{winningInts, myInts}
