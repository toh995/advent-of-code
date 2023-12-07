module Day05.Part02 where

import Control.Monad.Trans.State.Lazy
import Data.Either
import Data.Functor
import Data.Maybe
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Id = Int
type SourceId = Int
type DestId = Int

type InputRange = Range Id
type OutputRange = Range Id

data Range a = Range
  { start, end :: a
  }
  deriving (Eq, Show)

newtype CatMap = CatMap [CatMapItem]
  deriving (Eq, Show)

data CatMapItem = CatMapItem
  { sourceRange :: Range Id
  , sourceOffset :: Int
  }
  deriving (Eq, Show)

-- Range primitives
translate :: (Num a) => a -> Range a -> Range a
translate offset (Range{start, end}) =
  Range
    { start = start + offset
    , end = end + offset
    }

intersect :: (Ord a) => Range a -> Range a -> Maybe (Range a)
intersect
  (Range{start = s1, end = e1})
  (Range{start = s2, end = e2})
    | s1 < s2 =
        if e1 < s2
          then Nothing
          else Just Range{start = s2, end = min e1 e2}
    | s1 > e2 = Nothing
    | otherwise = Just Range{start = s1, end = min e1 e2}

diff :: (Num a, Ord a) => Range a -> Range a -> [Range a]
diff
  (Range{start = s1, end = e1})
  (Range{start = s2, end = e2}) =
    left ++ right
   where
    left =
      if s1 >= s2
        then []
        else [Range{start = s1, end = min e1 (s2 - 1)}]
    right =
      if e1 <= e2
        then []
        else [Range{start = max s1 (e2 + 1), end = e1}]

minR :: (Ord a) => [Range a] -> a
minR = minimum . map start

-- Conversion logic
part2 :: String -> Int
part2 inputStr =
  let (seedRanges, catMaps) = parseAll inputStr
   in minR $ fullConvert seedRanges catMaps

fullConvert :: [InputRange] -> [CatMap] -> [OutputRange]
fullConvert = foldl convert

convert :: [InputRange] -> CatMap -> [OutputRange]
convert inputs (CatMap catMapItems) =
  let (inputs', outputs') =
        execState
          (mapM convertS catMapItems)
          (inputs, [])
   in inputs' ++ outputs'

convertS :: CatMapItem -> State ([InputRange], [OutputRange]) ()
convertS (CatMapItem{sourceRange, sourceOffset}) = do
  (inputs, outputs) <- get
  let outputs' =
        map (translate sourceOffset)
          . mapMaybe (intersect sourceRange)
          $ inputs
  let inputs' = concatMap (`diff` sourceRange) inputs
  put (inputs', outputs' ++ outputs)

-- Parsing logic
type Parser = Parsec Void String

parseAll :: String -> ([Range Id], [CatMap])
parseAll =
  fromRight ([], [])
    . runParser allP ""

allP :: Parser ([Range Id], [CatMap])
allP =
  (,)
    <$> (rangesP <* doubleNewline)
    <*> (catMapP `sepBy` doubleNewline)

rangesP :: Parser [Range Id]
rangesP =
  string "seeds: "
    *> rangeP `sepBy` hspace

rangeP :: Parser (Range Id)
rangeP = do
  start <- L.decimal <* hspace
  rangeLen <- L.decimal
  pure $ Range{start, end = start + rangeLen - 1}

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
  pure $
    CatMapItem
      { sourceRange = Range{start = sourceStart, end = sourceStart + rangeLen - 1}
      , sourceOffset = destStart - sourceStart
      }

doubleNewline :: Parser [Char]
doubleNewline = string "\n\n"

singleNewline :: Parser Char
singleNewline = notFollowedBy doubleNewline *> char '\n'
