module Day02.Main where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Ix (range)
import Text.Parsec (Parsec, many, many1, try)
import qualified Text.Parsec.Char as C
import Text.Parsec.String (parseFromFile)

filePath :: FilePath
filePath = "src/Day02/data.txt"

main :: IO ()
main = do
    intervals <-
        parseFromFile intervalsP filePath
            <&> unwrapEither
    let part1Answer = solve isInvalid1 intervals
    putStrLn $ "PART1: " ++ show part1Answer
    let part2Answer = solve isInvalid2 intervals
    putStrLn $ "PART2: " ++ show part2Answer

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Right b) = b
unwrapEither (Left a) = error $ show a

solve :: (Int -> Bool) -> [(Int, Int)] -> Int
solve isInvalid =
    concatMap range
        >>> filter isInvalid
        >>> sum

isInvalid1 :: Int -> Bool
isInvalid1 =
    show
        >>> splitHalf
        >>> uncurry (==)

isInvalid2 :: Int -> Bool
isInvalid2 num =
    let
        numStr = show num
        numLen = length numStr
        chunkSizes =
            [1 .. length numStr `div` 2]
                & filter ((== 0) . (numLen `mod`))
        chunkss = (`divvy` numStr) <$> chunkSizes
     in
        any allEqual chunkss

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual (x : xs) = all (== x) xs

divvy :: Int -> [a] -> [[a]]
divvy _ [] = []
divvy chunkSize xs =
    let (chunk, rest) = splitAt chunkSize xs
     in chunk : divvy chunkSize rest

splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt (length xs `div` 2) xs

type Parser = Parsec String ()

intervalsP :: Parser [(Int, Int)]
intervalsP = sepBy1 intervalP (C.char ',')

intervalP :: Parser (Int, Int)
intervalP = do
    lowerBound <- intP
    _ <- C.char '-'
    upperBound <- intP
    pure (lowerBound, upperBound)

intP :: Parser Int
intP = read <$> many1 (try C.digit)

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 aP sepP = do
    a <- aP
    as <- many (try $ sepP >> aP)
    pure (a : as)
