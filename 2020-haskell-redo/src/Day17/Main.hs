module Day17.Main where

import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

filePath :: String
filePath = "src/Day17/data.txt"

main :: IO ()
main = do
    parsed <- readFile filePath <&> parseActive
    let part1Answer = part1 parsed
    putStrLn ("PART1: " ++ show part1Answer)
    let part2Answer = part2 parsed
    putStrLn ("PART2: " ++ show part2Answer)

parseActive :: String -> [(Int, Int)]
parseActive s =
    [ (i, j)
    | (i, row) <- zip [0 ..] rows
    , (j, cellVal) <- zip [0 ..] row
    , cellVal == '#'
    ]
  where
    rows = lines s

part1 :: [(Int, Int)] -> Int
part1 input =
    S.size finalActiveCoords
  where
    startActiveCoords =
        S.fromList $
            map
                (\(x, y) -> [x, y, 0])
                input
    finalActiveCoords = applyN 6 doCycle startActiveCoords

part2 :: [(Int, Int)] -> Int
part2 input =
    S.size finalActiveCoords
  where
    startActiveCoords =
        S.fromList $
            map
                (\(x, y) -> [x, y, 0, 0])
                input
    finalActiveCoords = applyN 6 doCycle startActiveCoords

doCycle :: Set [Int] -> Set [Int]
doCycle activeCoords =
    activeCoords `S.difference` coordsToRemove
        & S.union coordsToAdd
  where
    coordsToRemove =
        activeCoords
            & S.filter
                ( \activeCoord ->
                    let activeNeighborCount =
                            neighbors activeCoord
                                & filter (`S.member` activeCoords)
                                & length
                     in not $ activeNeighborCount `elem` [2, 3]
                )
    coordsToAdd =
        S.fromList
            [ inactiveCoord
            | (inactiveCoord, activeNeighbors) <- M.toList inactiveToActive
            , S.size activeNeighbors == 3
            ]
    inactiveToActive =
        M.fromListWith
            S.union
            [ (neighbor, S.singleton activeCoord)
            | activeCoord <- S.toList activeCoords
            , neighbor <- neighbors activeCoord
            , not $ neighbor `S.member` activeCoords
            ]

neighbors :: [Int] -> [[Int]]
neighbors [] = []
neighbors startCoords =
    go startCoords
        & filter (/= startCoords)
  where
    go [] = [[]]
    go (x : xs) =
        [ (x + delta) : suffix
        | delta <- [-1, 0, 1]
        , suffix <- go xs
        ]

applyN :: Int -> (a -> a) -> a -> a
applyN n f x
    | n <= 0 = x
    | otherwise = applyN (n - 1) f (f x)
