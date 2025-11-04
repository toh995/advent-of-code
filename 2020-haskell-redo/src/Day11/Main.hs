{-# LANGUAGE LambdaCase #-}

module Day11.Main where

import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Foldable (find)
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)

data Tile = OccupiedSeat | EmptySeat | Floor
    deriving (Eq)

type CoordPair = (Int, Int)

type Grid = Map CoordPair Tile

filePath :: String
filePath = "src/Day11/data.txt"

main :: IO ()
main = do
    inputStr <- readFile filePath
    let grid =
            case parseGrid inputStr of
                Just val -> val
                Nothing -> error "could not parse!"
    let part1Answer = solve doRound1 grid
    putStrLn ("PART 1: " ++ show part1Answer)

    let part2Answer = solve doRound2 grid
    putStrLn ("PART 2: " ++ show part2Answer)

parseGrid :: String -> Maybe Grid
parseGrid s = do
    rows <- lines s & mapM (mapM parseTile)
    pure $
        M.fromList
            [ ((i, j), tile)
            | (i, row) <- zip [0 ..] rows
            , (j, tile) <- zip [0 ..] row
            ]

parseTile :: Char -> Maybe Tile
parseTile '#' = Just OccupiedSeat
parseTile 'L' = Just EmptySeat
parseTile '.' = Just Floor
parseTile _ = Nothing

solve :: (Grid -> Grid) -> Grid -> Int
solve doRound grid =
    finalGrid
        & M.elems
        & filter (== OccupiedSeat)
        & length
  where
    (finalGrid, _) =
        iterate doRound grid
            & adjacentPairs
            & find (uncurry (==))
            & fromJust

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs (x : y : rest) = [(x, y)] ++ adjacentPairs (y : rest)
adjacentPairs _ = []

doRound1 :: Grid -> Grid
doRound1 grid =
    M.mapWithKey f grid
  where
    f (i, j) currTile
        | currTile == EmptySeat && numOccupiedNeighbors == 0 = OccupiedSeat
        | currTile == OccupiedSeat && numOccupiedNeighbors >= 4 = EmptySeat
        | otherwise = currTile
      where
        numOccupiedNeighbors =
            neighborCoords
                & filter ((grid M.!?) >>> (== Just OccupiedSeat))
                & length
        neighborCoords =
            [ (i + di, j + dj)
            | di <- [-1, 0, 1]
            , dj <- [-1, 0, 1]
            , (di, dj) /= (0, 0)
            ]

doRound2 :: Grid -> Grid
doRound2 grid =
    M.mapWithKey f grid
  where
    f currCoord currTile
        | currTile == EmptySeat && numOccupiedNeighbors == 0 = OccupiedSeat
        | currTile == OccupiedSeat && numOccupiedNeighbors >= 5 = EmptySeat
        | otherwise = currTile
      where
        numOccupiedNeighbors =
            neighborTiles
                & filter (== OccupiedSeat)
                & length
        neighborTiles =
            do
                di <- [-1, 0, 1]
                dj <- [-1, 0, 1]
                guard $ (di, dj) /= (0, 0)
                let visibleSeat =
                        currCoord
                            & iterate (\(i, j) -> (i + di, j + dj))
                            & drop 1 -- skip the start coord
                            & map (grid M.!?)
                            & takeWhile isJust
                            & map fromJust
                            & find
                                ( \case
                                    OccupiedSeat -> True
                                    EmptySeat -> True
                                    Floor -> False
                                )
                case visibleSeat of
                    Just visibleSeat' -> pure visibleSeat'
                    Nothing -> []
