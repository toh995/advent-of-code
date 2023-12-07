module Day05.Part01 where

import Control.Applicative (asum)
import Data.Either
import Data.Functor
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Id = Int

newtype CatMap = CatMap [CatMapItem]
  deriving (Eq, Show)

data CatMapItem = CatMapItem
  { sourceStart, destStart, rangeLen :: Int
  }
  deriving (Eq, Show)

part1 :: String -> Id
part1 inputStr =
  let (ids, catMaps) = parseAll inputStr
   in minimum $
        map (`fullConvert` catMaps) ids

-- Sequentially thread the given id through all of the maps
fullConvert :: Id -> [CatMap] -> Id
fullConvert = foldl convert

convert :: Id -> CatMap -> Id
convert n (CatMap mapItems) =
  fromMaybe n
    . asum
    . map (convert' n)
    $ mapItems
 where
  convert' :: Id -> CatMapItem -> Maybe Id
  convert' n' (CatMapItem{sourceStart, destStart, rangeLen})
    | sourceStart <= n' && n' <= sourceStart + rangeLen = Just $ (n' - sourceStart) + destStart
    | otherwise = Nothing

-- Parsing logic
type Parser = Parsec Void String

parseAll :: String -> ([Id], [CatMap])
parseAll =
  fromRight ([], [])
    . runParser allP ""

allP :: Parser ([Id], [CatMap])
allP =
  (,)
    <$> (seedIdsP <* doubleNewline)
    <*> (catMapP `sepBy` doubleNewline)

seedIdsP :: Parser [Id]
seedIdsP =
  string "seeds: "
    *> L.decimal `sepBy` hspace

catMapP :: Parser CatMap
catMapP =
  anySingle `skipManyTill` singleNewline
    *> catMapItemP `sepEndBy` singleNewline
    <&> CatMap

catMapItemP :: Parser CatMapItem
catMapItemP = do
  destStart <- L.decimal <* hspace
  sourceStart <- L.decimal <* hspace
  rangeLen <- L.decimal <* hspace
  pure CatMapItem{destStart, sourceStart, rangeLen}

doubleNewline :: Parser [Char]
doubleNewline = string "\n\n"

singleNewline :: Parser Char
singleNewline = notFollowedBy doubleNewline *> char '\n'
