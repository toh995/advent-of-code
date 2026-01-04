module Day23.Main where

import Data.Array.IArray (Array)
import Data.Array.IArray qualified as A
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust)
import Data.Set qualified as S

filePath :: FilePath
filePath = "src/Day23/data.txt"

main :: IO ()
main = do
    inputStr <- readFile filePath
    let matrix = parseMatrix inputStr
    let part1Answer = part1 matrix
    putStrLn $ "PART1: " ++ show part1Answer
    let part2Answer = part2 matrix
    putStrLn $ "PART2: " ++ show part2Answer

type Matrix a = Array (Int, Int) a

data Tile
    = Path
    | Forest
    | Slope Direction
    deriving (Eq)

data Direction = North | South | East | West
    deriving (Eq)

type I = Int
type J = Int
type Coord = (I, J)

part1 :: Matrix Tile -> Int
part1 = solve neighborCoords
  where
    neighborCoords matrix coord@(i, j) =
        filter
            (isPathOrSlope matrix)
            ret
      where
        ret = case matrix A.!? coord of
            Nothing -> []
            Just Forest -> []
            Just Path ->
                [ (i + 1, j)
                , (i - 1, j)
                , (i, j + 1)
                , (i, j - 1)
                ]
            Just (Slope North) -> [(i - 1, j)]
            Just (Slope South) -> [(i + 1, j)]
            Just (Slope East) -> [(i, j + 1)]
            Just (Slope West) -> [(i, j - 1)]

part2 :: Matrix Tile -> Int
part2 = solve neighborCoords
  where
    neighborCoords matrix coord@(i, j)
        | isPathOrSlope matrix coord =
            filter
                (isPathOrSlope matrix)
                [ (i + 1, j)
                , (i - 1, j)
                , (i, j + 1)
                , (i, j - 1)
                ]
        | otherwise = []

solve :: (Matrix Tile -> Coord -> [Coord]) -> Matrix Tile -> Int
solve neighborCoords matrix =
    fromJust $ go startCoord' 0 S.empty
  where
    weightedGraph = newWeightedGraph neighborCoords matrix
    startCoord' = fromJust $ startCoord matrix
    endCoord' = fromJust $ endCoord matrix
    go currCoord currCost seen
        | currCoord == endCoord' = Just currCost
        | currCoord `S.member` seen = Nothing
        | otherwise =
            maximumMay $
                catMaybes
                    [ go neighbor (currCost + weight) (S.insert currCoord seen)
                    | (neighbor, weight) <- weightedNeighbors currCoord weightedGraph
                    ]

startCoord :: Matrix Tile -> Maybe Coord
startCoord matrix = findPathCoord iMin matrix
  where
    ((iMin, _), _) = A.bounds matrix

endCoord :: Matrix Tile -> Maybe Coord
endCoord matrix = findPathCoord iMax matrix
  where
    (_, (iMax, _)) = A.bounds matrix

findPathCoord :: I -> Matrix Tile -> Maybe Coord
findPathCoord targetI matrix =
    case retCoords of
        [ret] -> Just ret
        _ -> Nothing
  where
    retCoords =
        [ (targetI, j)
        | j <- [jMin .. jMax]
        , Path == matrix A.! (targetI, j)
        ]
    ((_, jMin), (_, jMax)) = A.bounds matrix

isPathOrSlope :: Matrix Tile -> Coord -> Bool
isPathOrSlope matrix coord =
    case matrix A.!? coord of
        Just Path -> True
        Just (Slope _) -> True
        _ -> False

newtype WeightedGraph = WeightedGraph
    { adjList :: Map Coord [(Coord, Weight)]
    }

type Weight = Int

newWeightedGraph :: (Matrix Tile -> Coord -> [Coord]) -> Matrix Tile -> WeightedGraph
newWeightedGraph neighborCoords matrix =
    WeightedGraph
        { adjList =
            M.fromListWith
                (++)
                [ (from, [(to, weight)])
                | ((from, to), weight) <- M.assocs edgeWeights
                ]
        }
  where
    edgeWeights =
        M.fromListWith
            max
            [ ((startNode, destNode), weight)
            | startNode <- nodes
            , neighborCoord <- neighborCoords matrix startNode
            , Just (destNode, weight) <- [walkEdge startNode neighborCoord]
            ]
    nodes = filter isNode (A.indices matrix)
    isNode coord =
        coord == startCoord'
            || coord == endCoord'
            || case neighborCoords matrix coord of
                (_ : _ : _ : _) -> True
                _ -> False
    startCoord' = fromJust $ startCoord matrix
    endCoord' = fromJust $ endCoord matrix
    walkEdge node nextCoord =
        go
            nextCoord
            1
            (S.singleton node)
      where
        go currCoord currWeight seen
            | isNode currCoord = Just (currCoord, currWeight)
            | [neighborCoord] <- unvisitedNeighbors =
                go
                    neighborCoord
                    (currWeight + 1)
                    (S.insert currCoord seen)
            | otherwise = Nothing
          where
            unvisitedNeighbors =
                neighborCoords matrix currCoord
                    & filter (`S.notMember` seen)

weightedNeighbors :: Coord -> WeightedGraph -> [(Coord, Weight)]
weightedNeighbors coord WeightedGraph{adjList} =
    M.findWithDefault [] coord adjList

maximumMay :: (Ord a) => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay xs = Just $ maximum xs

parseMatrix :: String -> Matrix Tile
parseMatrix s =
    A.array
        arrayBounds
        [ ((i, j), parseTile char)
        | (i, row) <- zip [0 ..] rows
        , (j, char) <- zip [0 ..] row
        ]
  where
    rows = lines s
    arrayBounds =
        ( (0, 0)
        , (length rows - 1, length (head rows) - 1)
        )

parseTile :: Char -> Tile
parseTile '.' = Path
parseTile '#' = Forest
parseTile '^' = Slope North
parseTile '>' = Slope East
parseTile 'v' = Slope South
parseTile '<' = Slope West
parseTile c = error $ "could not parse tile: invalid tile char " ++ [c]
