{-# LANGUAGE NamedFieldPuns #-}

module Day12.Main where

import Data.Function ((&))
import Data.Functor ((<&>))
import Text.Parsec (Parsec, many, many1, try)
import qualified Text.Parsec.Char as C
import Text.Parsec.String (parseFromFile)

filePath :: FilePath
filePath = "src/Day12/data.txt"

main :: IO ()
main = do
    (shapeAreas, regions) <-
        parseFromFile allP filePath
            <&> unwrapEither
    let part1Answer = part1 shapeAreas regions
    putStrLn $ "PART1: " ++ show part1Answer

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Left a) = error $ show a
unwrapEither (Right b) = b

part1 :: [ShapeArea] -> [Region] -> Int
part1 shapeAreas regions =
    regions
        & filter (isValidRegion shapeAreas)
        & length

isValidRegion :: [ShapeArea] -> Region -> Bool
isValidRegion shapeAreas Region{len, width, shapeCounts} =
    requiredArea <= availableArea
  where
    availableArea = len * width
    requiredArea =
        sum $
            zipWith
                (*)
                shapeCounts
                shapeAreas

type Parser = Parsec String ()

type ShapeArea = Int
data Region = Region
    { len :: Int
    , width :: Int
    , shapeCounts :: [Int]
    }

allP :: Parser ([ShapeArea], [Region])
allP = do
    shapeAreas <- sepBy1 shapeAreaP (C.newline >> C.newline)
    _ <- C.newline >> C.newline
    regions <- sepBy1 regionP C.newline
    pure (shapeAreas, regions)

shapeAreaP :: Parser ShapeArea
shapeAreaP = do
    _ <- intP >> C.string ":\n"
    shapeLines <- sepBy1 (many1 (try $ C.oneOf ".#")) C.newline
    let shapeArea =
            shapeLines
                & concatMap (filter (== '#'))
                & length
    pure shapeArea

regionP :: Parser Region
regionP = do
    width <- intP
    _ <- C.char 'x'
    len <- intP
    _ <- C.string ": "
    shapeCounts <- sepBy1 intP (C.char ' ')
    pure Region{len, width, shapeCounts}

intP :: Parser Int
intP = read <$> many1 (try C.digit)

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 aP sepP = do
    a <- aP
    as <- many (try $ sepP >> aP)
    pure (a : as)
