module Day12.Main where

import Control.Category ((>>>))
import Data.Array.IArray (Array)
import Data.Array.IArray qualified as A
import Data.Foldable (toList)
import Data.Set (Set)
import Data.Set qualified as S

filePath :: FilePath
filePath = "src/Day12/data.txt"

main :: IO ()
main = do
    matrix <- parseMatrix <$> readFile filePath
    let part1Answer = part1 matrix
    putStrLn ("PART1: " ++ show part1Answer)
    let part2Answer = part2 matrix
    putStrLn ("PART2: " ++ show part2Answer)

type Matrix = Array Coord Char

type Coord = (Int, Int)

newtype Region = Region {coordSet :: Set Coord}

part1 :: Matrix -> Int
part1 =
    sum
        . map cost1
        . regions

part2 :: Matrix -> Int
part2 =
    sum
        . map cost2
        . regions

regions :: Matrix -> [Region]
regions matrix =
    go
        allCoords
        S.empty
        []
  where
    allCoords = A.indices matrix
    go [] _ acc = acc
    go (coord : nextCoords) seenCoords acc
        | coord `S.member` seenCoords = go nextCoords seenCoords acc
        | otherwise =
            let region = regionFrom coord matrix
                nextSeenCoords = S.union seenCoords (coordSet region)
             in go
                    nextCoords
                    nextSeenCoords
                    (region : acc)

regionFrom :: Coord -> Matrix -> Region
regionFrom startCoord matrix =
    Region{coordSet}
  where
    coordSet =
        case matrix A.!? startCoord of
            Nothing -> S.empty
            Just tile -> dfs tile [startCoord] S.empty

    dfs _ [] seen = seen
    dfs tile (currCoord : stack) seen
        | currCoord `S.member` seen = dfs tile stack seen
        | matrix A.!? currCoord == Just tile =
            dfs
                tile
                (neighborCoords currCoord ++ stack)
                (S.insert currCoord seen)
        | otherwise = dfs tile stack seen

cost1 :: Region -> Int
cost1 =
    (*)
        <$> area
        <*> perimeter1

cost2 :: Region -> Int
cost2 =
    (*)
        <$> area
        <*> perimeter2

area :: Region -> Int
area = S.size . coordSet

perimeter1 :: Region -> Int
perimeter1 Region{coordSet} =
    sum
        (perimeterSingle <$> toList coordSet)
  where
    perimeterSingle =
        neighborCoords
            >>> filter (`S.notMember` coordSet)
            >>> length

perimeter2 :: Region -> Int
perimeter2 Region{coordSet} =
    sum
        (cornerCount <$> toList coordSet)
  where
    cornerCount =
        (+)
            <$> convexCornerCount
            <*> concaveCornerCount
    convexCornerCount coord =
        length $
            filter
                ( \(neighbor1, neighbor2) ->
                    neighbor1 `S.notMember` coordSet
                        && neighbor2 `S.notMember` coordSet
                )
                [ map2 ($ coord) (move [N], move [E])
                , map2 ($ coord) (move [E], move [S])
                , map2 ($ coord) (move [S], move [W])
                , map2 ($ coord) (move [W], move [N])
                ]
    concaveCornerCount coord =
        length $
            filter
                ( \(neighbor1, neighbor2, diag) ->
                    neighbor1 `S.member` coordSet
                        && neighbor2 `S.member` coordSet
                        && diag `S.notMember` coordSet
                )
                [ map3 ($ coord) (move [N], move [E], move [N, E])
                , map3 ($ coord) (move [E], move [S], move [E, S])
                , map3 ($ coord) (move [S], move [W], move [S, W])
                , map3 ($ coord) (move [W], move [N], move [W, N])
                ]

data Direction = N | S | E | W
    deriving (Bounded, Enum)

allDirections :: [Direction]
allDirections = [minBound .. maxBound]

move :: [Direction] -> Coord -> Coord
move = flip $ foldl (flip step)

step :: Direction -> Coord -> Coord
step N (i, j) = (i - 1, j)
step S (i, j) = (i + 1, j)
step E (i, j) = (i, j + 1)
step W (i, j) = (i, j - 1)

neighborCoords :: Coord -> [Coord]
neighborCoords coord =
    map
        ((`move` coord) . (: []))
        allDirections

map2 :: (a -> b) -> (a, a) -> (b, b)
map2 f (x, y) = (f x, f y)

map3 :: (a -> b) -> (a, a, a) -> (b, b, b)
map3 f (x, y, z) = (f x, f y, f z)

parseMatrix :: String -> Matrix
parseMatrix str =
    A.array
        ((0, 0), (maxI, maxJ))
        [ ((i, j), char)
        | (i, row) <- zip [0 ..] rows
        , (j, char) <- zip [0 ..] row
        ]
  where
    rows = lines str
    maxI = length rows - 1
    maxJ = length (head rows) - 1
