{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Day09.Main where

import Control.Category ((>>>))
import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.IntMap as IM
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec (Parsec, many, many1, try)
import qualified Text.Parsec.Char as C
import Text.Parsec.String (parseFromFile)

filePath :: FilePath
filePath = "src/Day09/data.txt"

main :: IO ()
main = do
    points <-
        parseFromFile pointsP filePath
            <&> unwrapEither
    let part1Answer = part1 points
    putStrLn $ "PART1: " ++ show part1Answer
    let part2Answer = part2 points
    putStrLn $ "PART2: " ++ show part2Answer

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Left a) = error $ show a
unwrapEither (Right b) = b

part1 :: [Point] -> Int
part1 =
    pairs
        >>> map (area . Rectangle)
        >>> maximum

part2 :: [Point] -> Int
part2 points =
    pairs points
        & map Rectangle
        & filter (containsRect polygon)
        & map area
        & maximum
  where
    polygon = newPolygon points

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a : as) = ((a,) <$> as) ++ pairs as

type Point = (Int, Int)

newtype Rectangle = Rectangle
    { oppositeCorners :: (Point, Point)
    }

area :: Rectangle -> Int
area rect =
    (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)
  where
    ((x1, y1), (x2, y2)) = oppositeCorners rect

rectEdges :: Rectangle -> [[Point]]
rectEdges rect =
    [ [(x, y1) | x <- xs]
    , [(x, y2) | x <- xs]
    , [(x1, y) | y <- ys]
    , [(x2, y) | y <- ys]
    ]
  where
    ((x1, y1), (x2, y2)) = oppositeCorners rect
    xs = [min x1 x2 .. max x1 x2]
    ys = [min y1 y2 .. max y1 y2]

data Polygon = Polygon
    { cPolygon :: CPolygon
    , compress :: Point -> Point
    }

newtype CPolygon = CPolygon {outsideCoords :: Set Point}

newPolygon :: [Point] -> Polygon
newPolygon vertices =
    Polygon
        { cPolygon = newCPolygon (compress <$> vertices)
        , compress
        }
  where
    compress (x, y) =
        ( xCompressMap IM.! x
        , yCompressMap IM.! y
        )
    xs = fst <$> vertices
    ys = snd <$> vertices
    xCompressMap =
        IM.fromList $
            zip
                (sort . uniq $ xs)
                [0 ..]
    yCompressMap =
        IM.fromList $
            zip
                (sort . uniq $ ys)
                [0 ..]

newCPolygon :: [Point] -> CPolygon
newCPolygon cVertices =
    CPolygon
        { outsideCoords = floodFill [startPoint] S.empty
        }
  where
    xs = fst <$> cVertices
    ys = snd <$> cVertices
    (xMin, xMax) = (minimum xs, maximum xs)
    (yMin, yMax) = (minimum ys, maximum ys)
    startPoint = (xMin - 1, yMin - 1)

    borderPointSet = S.fromList borderPoints
    borderPoints =
        concat $
            zipWith
                edge
                cVertices
                (drop 1 cVertices ++ take 1 cVertices)

    isOutside p@(x, y) =
        not (p `S.member` borderPointSet)
            && (xMin - 1) <= x
            && x <= (xMax + 1)
            && (yMin - 1) <= y
            && y <= (yMax + 1)

    floodFill [] seen = seen
    floodFill (p : stack) seen
        | p `S.member` seen = floodFill stack seen
        | isOutside p = floodFill (neighbors p ++ stack) (S.insert p seen)
        | otherwise = floodFill stack seen

neighbors :: Point -> [Point]
neighbors (x, y) =
    [ (x, y + 1)
    , (x, y - 1)
    , (x + 1, y)
    , (x - 1, y)
    ]

edge :: Point -> Point -> [Point]
edge (x1, y1) (x2, y2)
    | x1 == x2 = [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
    | y1 == y2 = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
    | otherwise = error $ "IMPOSSIBLE EDGE with points " ++ show (x1, y1) ++ " and " ++ show (x2, y2)

containsRect :: Polygon -> Rectangle -> Bool
containsRect Polygon{cPolygon, compress} rect =
    containsRectC cPolygon cRect
  where
    cRect =
        Rectangle
            { oppositeCorners =
                bimap
                    compress
                    compress
                    (oppositeCorners rect)
            }

containsRectC :: CPolygon -> Rectangle -> Bool
containsRectC CPolygon{outsideCoords} rect =
    all
        ( all
            (`S.notMember` outsideCoords)
        )
        (rectEdges rect)

uniq :: (Ord a) => [a] -> [a]
uniq = S.fromList >>> toList

type Parser = Parsec String ()

pointsP :: Parser [Point]
pointsP = sepBy1 pointP C.newline

pointP :: Parser Point
pointP = do
    x' <- intP
    _ <- C.char ','
    y' <- intP
    pure (x', y')

intP :: Parser Int
intP = read <$> many1 (try C.digit)

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 aP sepP = do
    a <- aP
    as <- many (try $ sepP >> aP)
    pure (a : as)
