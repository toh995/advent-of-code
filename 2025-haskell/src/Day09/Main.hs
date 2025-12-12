{-# LANGUAGE TupleSections #-}

module Day09.Main where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sortOn)
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
    -- let part2Answer = part2 points
    -- putStrLn $ "PART2: " ++ show part2Answer
    pure ()

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

-- area :: (Num a) => (a, a) -> (a, a) -> a
-- area (x1, y1) (x2, y2) =
--     abs $
--         (x1 - x2 + 1) * (y1 - y2 + 1)

area :: Rectangle -> Int
area rect =
    abs $
        (x1 - x2 + 1) * (y1 - y2 + 1)
  where
    ((x1, y1), (x2, y2)) = oppositeCorners rect

data Polygon = Polygon
    { sortedVerticalEdges :: [VerticalEdge]
    , sortedHorizontalEdges :: [HorizontalEdge]
    }

data VerticalEdge = VerticalEdge
    { x :: Int
    , yRange :: (Int, Int)
    }

data HorizontalEdge = HorizontalEdge
    { y :: Int
    , xRange :: (Int, Int)
    }

newPolygon :: [Point] -> Polygon
newPolygon points =
    go points [] []
  where
    go ((x1, y1) : p2@(x2, y2) : ps) ves hes
        | x1 == x2 = go (p2 : ps) (ve : ves) hes
        | y1 == y2 = go (p2 : ps) ves (he : hes)
        | otherwise = error "invalid points list for new polygon"
      where
        ve = VerticalEdge{x = x1, yRange = (min y1 y2, max y1 y2)}
        he = HorizontalEdge{y = y1, xRange = (min x1 x2, max x1 x2)}
    go _ ves hes =
        Polygon
            { sortedVerticalEdges = sortOn x ves
            , sortedHorizontalEdges = sortOn y hes
            }

containsRect :: Polygon -> Rectangle -> Bool
containsRect polygon rect =
    all (containsPoint polygon) [corner1, corner2]
        && not (intersectsEdge polygon rect)
  where
    ((x1, y1), (x2, y2)) = oppositeCorners rect
    corner1 = (x1, y2)
    corner2 = (x2, y1)

containsPoint :: Polygon -> Point -> Bool
containsPoint polygon point = _

intersectsEdge :: Polygon -> Rectangle -> Bool
intersectsEdge _ _ = _

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
