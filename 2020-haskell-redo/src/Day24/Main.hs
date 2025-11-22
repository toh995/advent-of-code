{-# LANGUAGE MultiWayIf #-}

module Day24.Main where

import Control.Category ((>>>))
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

data Direction
    = East
    | Southeast
    | Southwest
    | West
    | Northwest
    | Northeast
    deriving (Bounded, Enum)

allDirections :: [Direction]
allDirections = [minBound .. maxBound]

type Tile = (Int, Int)

filePath :: String
filePath = "src/Day24/data.txt"

main :: IO ()
main = do
    dirGroups <-
        readFile filePath
            <&> ( lines
                    >>> mapM parseRow
                    >>> unwrapEither
                )
    let part1Answer = part1 dirGroups
    putStrLn $ "PART1: " ++ show part1Answer
    let part2Answer = part2 dirGroups
    putStrLn ("PART2: " ++ show part2Answer)

part1 :: [[Direction]] -> Int
part1 = S.size . blackTiles

part2 :: [[Direction]] -> Int
part2 dirGroup =
    S.size finalBlackTiles
  where
    finalBlackTiles = applyN 100 doConway (blackTiles dirGroup)

blackTiles :: [[Direction]] -> Set Tile
blackTiles dirGroups =
    flipCounts
        & M.filter odd
        & M.keysSet
  where
    flipCounts =
        M.fromListWith
            (+)
            [(coord, 1 :: Int) | coord <- coordsToFlip]
    coordsToFlip = walk (0, 0) <$> dirGroups

walk :: Tile -> [Direction] -> Tile
walk = foldl step

step :: Tile -> Direction -> Tile
step (x, y) East = (x + 2, y)
step (x, y) Southeast = (x + 1, y - 1)
step (x, y) Southwest = (x - 1, y - 1)
step (x, y) West = (x - 2, y)
step (x, y) Northwest = (x - 1, y + 1)
step (x, y) Northeast = (x + 1, y + 1)

applyN :: Int -> (a -> a) -> a -> a
applyN n f a
    | n < 0 = error $ "cannot apply f a negative number of times; tried to execute " ++ show n ++ " times."
    | n == 0 = a
    | otherwise = applyN (n - 1) f (f a)

doConway :: Set Tile -> Set Tile
doConway blackTiles' =
    blackTiles'
        & (`S.difference` blackToWhite)
        & S.union whiteToBlack
  where
    blackNeighborCounts =
        M.fromListWith
            (+)
            [ (neighbor, 1 :: Int)
            | blackTile <- toList blackTiles'
            , neighbor <- neighbors blackTile
            ]
    blackToWhite =
        blackTiles'
            & S.filter
                ( \t ->
                    let numBlackNeighbors = blackNeighborCounts & M.findWithDefault 0 t
                     in if
                            | numBlackNeighbors == 0 -> True
                            | numBlackNeighbors > 2 -> True
                            | otherwise -> False
                )
    whiteToBlack =
        S.fromList
            [ tile
            | (tile, numBlackNeighbors) <- M.toList blackNeighborCounts
            , tile `S.notMember` blackTiles'
            , numBlackNeighbors == 2
            ]

neighbors :: Tile -> [Tile]
neighbors coord = step coord <$> allDirections

parseRow :: String -> Either String [Direction]
parseRow [] = Right []
parseRow ('s' : 'e' : cs) = (Southeast :) <$> parseRow cs
parseRow ('s' : 'w' : cs) = (Southwest :) <$> parseRow cs
parseRow ('n' : 'w' : cs) = (Northwest :) <$> parseRow cs
parseRow ('n' : 'e' : cs) = (Northeast :) <$> parseRow cs
parseRow ('e' : cs) = (East :) <$> parseRow cs
parseRow ('w' : cs) = (West :) <$> parseRow cs
parseRow s = Left $ "could not parse string " ++ s

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Right b) = b
unwrapEither (Left a) = error $ show a
