module Day07.Part02 where

import Data.Either
import Data.Functor
import Data.HashMap.Lazy qualified as HashMap
import Data.Hashable
import Data.List
import Data.Maybe
import Data.Void
import GHC.Generics (Generic)
import Safe
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- The clause `deriving (Ord)` will auto-define a sort order,
-- based on order of appearance. For example:
-- Two < Three < ... < K < A
-- More info: https://www.haskell.org/onlinereport/haskell2010/haskellch11.html#x18-18300011.1
data Card = J | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Q | K | A
  deriving (Eq, Ord, Generic, Show)

instance Hashable Card

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Eq, Ord, Show)

-- Here, we explicitly define a custom comparator for the `Hand` type,
-- instead of relying on the auto-generated one i.e. `deriving (Ord)`.
data Hand = Hand
  { cards :: [Card]
  , bid :: Int
  }
  deriving (Eq, Show)

instance Ord Hand where
  compare h1 h2
    | type1 /= type2 = compare type1 type2
    | otherwise = compare (cards h1) (cards h2)
   where
    type1 = getType h1
    type2 = getType h2

getType :: Hand -> HandType
getType (Hand{cards})
  | sortedCounts == [5] = FiveOfAKind
  | sortedCounts == [1, 4] = FourOfAKind
  | sortedCounts == [2, 3] = FullHouse
  | sortedCounts == [1, 1, 3] = ThreeOfAKind
  | sortedCounts == [1, 2, 2] = TwoPair
  | sortedCounts == [1, 1, 1, 2] = OnePair
  | otherwise = HighCard
 where
  countMapNoJ =
    foldr
      (flip (HashMap.insertWith (+)) 1)
      HashMap.empty
      (filter (/= J) cards)
  sortedCountsNoJ = sort . HashMap.elems $ countMapNoJ
  numJs = length . filter (== J) $ cards
  sortedCounts =
    fromMaybe
      [numJs]
      (updateLast (+ numJs) sortedCountsNoJ)

updateLast :: (a -> a) -> [a] -> Maybe [a]
updateLast f as = do
  firsts <- initMay as
  final <- lastMay as
  pure $ firsts ++ [f final]

-- Main logic
filePath :: String
filePath = "src/Day07/data.txt"

main :: IO ()
main = do
  hands <- readHands
  let part2Answer = part2 hands
  putStrLn $ "PART 2: " ++ show part2Answer

readHands :: IO [Hand]
readHands =
  readFile filePath
    <&> fromRight []
      . runParser handsP ""

part2 :: [Hand] -> Int
part2 =
  sum
    . zipWith (*) [1 ..]
    . map bid
    . sort

-- Parsing logic
type Parser = Parsec Void String

handsP :: Parser [Hand]
handsP = handP `sepEndBy` newline

handP :: Parser Hand
handP = do
  cards <- many cardP <* hspace
  bid <- L.decimal
  pure Hand{cards, bid}

cardP :: Parser Card
cardP =
  (char 'J' $> J)
    <|> (char '2' $> Two)
    <|> (char '3' $> Three)
    <|> (char '4' $> Four)
    <|> (char '5' $> Five)
    <|> (char '6' $> Six)
    <|> (char '7' $> Seven)
    <|> (char '8' $> Eight)
    <|> (char '9' $> Nine)
    <|> (char 'T' $> Ten)
    <|> (char 'Q' $> Q)
    <|> (char 'K' $> K)
    <|> (char 'A' $> A)
