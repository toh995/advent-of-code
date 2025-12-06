module Day05.Main where

import Data.Array (Array)
import qualified Data.Array as A
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Maybe (isJust)
import Text.Parsec (Parsec, many, many1, try)
import qualified Text.Parsec.Char as C
import Text.Parsec.String (parseFromFile)

type Interval = (Integer, Integer)

filePath :: FilePath
filePath = "src/Day05/data.txt"

main :: IO ()
main = do
    (intervals, integers) <-
        parseFromFile mainP filePath
            <&> unwrapEither
    let part1Answer = part1 intervals integers
    putStrLn $ "PART1: " ++ show part1Answer
    let part2Answer = part2 intervals
    putStrLn $ "PART2: " ++ show part2Answer

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Right b) = b
unwrapEither (Left a) = error $ show a

part1 :: [Interval] -> [Integer] -> Int
part1 intervals =
    length
        . filter belongsTo
  where
    belongsTo searchTerm =
        isJust $
            binarySearch cmp lo hi
      where
        (lo, hi) = A.bounds intervalsArr
        cmp i
            | searchTerm < a = GT
            | searchTerm > b = LT
            | otherwise = EQ
          where
            (a, b) = intervalsArr A.! i
    intervalsArr =
        intervals
            & merge
            & sort
            & listToArray

part2 :: [Interval] -> Integer
part2 =
    sum
        . map ((+ 1) . uncurry subtract)
        . merge

listToArray :: [a] -> Array Int a
listToArray xs = A.listArray (0, length xs - 1) xs

binarySearch :: (Integral a) => (a -> Ordering) -> a -> a -> Maybe a
binarySearch cmp lo hi
    | lo <= hi =
        case cmp mid of
            EQ -> Just mid
            LT -> binarySearch cmp (mid + 1) hi
            GT -> binarySearch cmp lo (mid - 1)
    | otherwise = Nothing
  where
    mid = (lo + hi) `div` 2

merge :: [Interval] -> [Interval]
merge = foldr f [] . sort
  where
    f (a, b) [] = [(a, b)]
    f (a, b) ((c, d) : acc)
        | b < c = (a, b) : (c, d) : acc
        | otherwise = (a, max b d) : acc

type Parser = Parsec String ()

mainP :: Parser ([Interval], [Integer])
mainP = do
    intervals <- sepBy1 intervalP C.newline
    _ <- C.string "\n\n"
    integers <- sepBy1 integerP C.newline
    pure (intervals, integers)

intervalP :: Parser Interval
intervalP = do
    lowerBound <- integerP
    _ <- C.char '-'
    upperBound <- integerP
    pure (lowerBound, upperBound)

integerP :: Parser Integer
integerP = read <$> many1 (try C.digit)

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 aP sepP = do
    a <- aP
    as <- many (try $ sepP >> aP)
    pure (a : as)
