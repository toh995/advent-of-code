{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day08.Main where

import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.List (sortBy, sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (Down (..))
import Text.Parsec (Parsec, many, many1, try)
import qualified Text.Parsec.Char as C
import Text.Parsec.String (parseFromFile)

filePath :: FilePath
filePath = "src/Day08/data.txt"

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
unwrapEither (Right b) = b
unwrapEither (Left a) = error $ show a

part1 :: [(Int, Int, Int)] -> Int
part1 points =
    finalDS
        & sizes
        & M.elems
        & sortOn Down
        & take 3
        & product
  where
    startDS = singletons points
    finalDS =
        foldr
            (uncurry union)
            startDS
            pairs'
    pairs' =
        pairs points
            & sortBy (compare `on` uncurry (distance3 @_ @Double))
            & take 1_000

part2 :: [(Int, Int, Int)] -> Int
part2 points = x1 * x2
  where
    ((x1, _, _), (x2, _, _)) =
        findUnifyingPair
            (singletons points)
            pairsInit
    pairsInit =
        pairs points
            & sortBy (compare `on` uncurry (distance3 @_ @Double))
    findUnifyingPair ds (pair : nextPairs)
        | setCount nextDS == 1 = pair
        | otherwise = findUnifyingPair nextDS nextPairs
      where
        nextDS = uncurry union pair ds
    findUnifyingPair _ [] = error "couldn't find pair which causes unifies all points into a single set"

distance3 :: (Integral a, Floating b) => (a, a, a) -> (a, a, a) -> b
distance3 (x1, y1, z1) (x2, y2, z2) =
    sqrt . fromIntegral $
        sum
            [ (x1 - x2) ^ (2 :: Integer)
            , (y1 - y2) ^ (2 :: Integer)
            , (z1 - z2) ^ (2 :: Integer)
            ]

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = ((x,) <$> xs) ++ pairs xs

data DisjointSets a = DisjointSets
    { parents :: Map a a
    , sizes :: Map a Int
    }
    deriving (Show)

singletons :: (Ord a) => [a] -> DisjointSets a
singletons xs =
    DisjointSets
        { parents = M.fromList [(x, x) | x <- xs]
        , sizes = M.fromList [(x, 1) | x <- xs]
        }

setCount :: DisjointSets a -> Int
setCount DisjointSets{sizes} = M.size sizes

union :: (Ord a) => a -> a -> DisjointSets a -> DisjointSets a
union x y ds
    | rootX == rootY = ds2
    | otherwise =
        ds2
            { parents =
                parents ds2
                    & M.insert rootX' rootY'
            , sizes =
                sizes ds2
                    & M.adjust (+ sizes ds2 M.! rootX') rootY'
                    & M.delete rootX'
            }
  where
    (rootX, ds1) = root x ds
    (rootY, ds2) = root y ds1
    (rootX', rootY') =
        if sizes ds2 M.! rootX < sizes ds2 M.! rootY
            then (rootX, rootY)
            else (rootY, rootX)

root :: (Ord a) => a -> DisjointSets a -> (a, DisjointSets a)
root x ds
    | parent == x = (x, ds)
    | otherwise =
        let (root', ds1) = root parent ds
            ds2 = ds1{parents = parents ds1 & M.insert x root'}
         in (root', ds2)
  where
    parent = parents ds M.! x

type Parser = Parsec String ()

pointsP :: Parser [(Int, Int, Int)]
pointsP = sepBy1 pointP C.newline

pointP :: Parser (Int, Int, Int)
pointP = do
    x <- intP
    _ <- C.char ','
    y <- intP
    _ <- C.char ','
    z <- intP
    pure (x, y, z)

intP :: Parser Int
intP = read <$> many1 (try C.digit)

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 aP sepP = do
    a <- aP
    as <- many (try $ sepP >> aP)
    pure (a : as)
