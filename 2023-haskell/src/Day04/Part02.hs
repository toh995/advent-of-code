module Day04.Part02 where

import Data.Either
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Id = Int

data Card = Card
  { cardId :: Id
  , leftInts, rightInts :: [Int]
  }

data CardColl = CardColl
  { cards :: [Card]
  , idToCard :: IntMap Card
  , allIds :: IntSet
  , sizeForId :: Id -> Int
  , neighborsForId :: Int -> [Id]
  }

fromCards :: [Card] -> CardColl
fromCards cards =
  cardColl
 where
  cardColl =
    CardColl
      { cards
      , idToCard = IntMap.fromList . map (\c -> (cardId c, c)) $ cards
      , allIds = IntSet.fromList . map cardId $ cards
      , sizeForId = memoize (_sizeForId cardColl)
      , neighborsForId = memoize (_neighborsForId cardColl)
      }

totalSize :: CardColl -> Int
totalSize cardColl =
  sum
    . map (sizeForId cardColl)
    . IntSet.toList
    . allIds
    $ cardColl

-- Don't use this directly, instead use the memoized version
_sizeForId :: CardColl -> Id -> Int
_sizeForId cardColl k
  | not $ isValidId cardColl k = 0
  | otherwise =
      1
        + ( sum
              . map (sizeForId cardColl)
              $ neighborIds
          )
 where
  neighborIds = neighborsForId cardColl k

lookupCard :: CardColl -> Id -> Maybe Card
lookupCard (CardColl{idToCard}) k =
  IntMap.lookup k idToCard

-- Don't use this directly, instead use the memoized version
_neighborsForId :: CardColl -> Id -> [Id]
_neighborsForId cardColl k =
  fromMaybe
    []
    (neighborsForCard <$> lookupCard cardColl k)

neighborsForCard :: Card -> [Id]
neighborsForCard (Card{cardId, leftInts, rightInts}) =
  -- If we need more efficiency here, we can change from
  -- list intersection to set intersection
  let numMatches = length $ intersect leftInts rightInts
   in take numMatches [cardId + 1 ..]

isValidId :: CardColl -> Id -> Bool
isValidId (CardColl{allIds}) =
  flip IntSet.member allIds

-- Parsing Logic
type Parser = Parsec Void String

cardsP :: Parser [Card]
cardsP = cardP `sepEndBy` newline

cardP :: Parser Card
cardP = do
  cardId <- string "Card" *> hspace *> L.decimal
  _ <- char ':' *> hspace
  leftInts <- L.decimal `sepEndBy` hspace
  _ <- char '|' *> hspace
  rightInts <- L.decimal `sepEndBy` hspace
  pure Card{cardId, leftInts, rightInts}

-- Main Logic
part2 :: String -> Int
part2 inputStr =
  let cards = fromRight [] . runParser cardsP "" $ inputStr
      cardColl = fromCards cards
   in totalSize cardColl

-- Memoize Util
-- It's not the best, but it gets the job done
-- We could improve this by switching to a
-- more efficient backing data structure for
-- storing the memoiziation results (e.g. HashSet, tree, etc.)
-- We're currently using a linked list...
memoize :: (Int -> a) -> Int -> a
memoize f = (!!) $ map f [0 ..]
