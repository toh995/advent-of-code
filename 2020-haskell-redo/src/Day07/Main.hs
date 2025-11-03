module Day07.Main where

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Data.Function ((&))
import Data.Functor (($>))
import qualified Data.Map as M
import Day07.Graph (Graph)
import qualified Day07.Graph as G
import Text.Parsec (Parsec, anyToken, choice, many1, manyTill, parse, sepBy, sepEndBy, try)
import qualified Text.Parsec.Char as C

filePath :: String
filePath = "src/Day07/data.txt"

main :: IO ()
main = do
    inputStr <- readFile filePath
    let rows =
            case parse rowsP filePath inputStr of
                Right val -> val
                Left err -> error $ show err
    let graph = G.fromList rows

    let part1Answer = part1 graph
    putStrLn ("PART 1: " ++ show part1Answer)

    let part2Answer = part2 graph
    putStrLn ("PART 2: " ++ show part2Answer)

part1 :: Graph Node -> Int
part1 =
    G.inverse
        >>> G.dfs "shiny gold"
        >>> length
        >>> (subtract 1) -- remove the "start node" from the DFS count

part2 :: Graph Node -> Int
part2 graph =
    memoized M.! "shiny gold"
  where
    memoized = M.fromList [(node, countBags node) | node <- G.nodes graph]
    countBags node =
        G.weightedNeighbors node graph
            & map (\(neighbor, weight) -> weight * (1 + memoized M.! neighbor))
            & sum

type Node = String

rowsP :: Parsec String () [(Node, [(Node, Int)])]
rowsP = sepEndBy rowP (C.char '\n')

rowP :: Parsec String () (Node, [(Node, Int)])
rowP = do
    fromNode <- manyTill anyToken $ try (C.string " bags contain ")
    weightedNeighbors <- neighborsP
    _ <- C.char '.'
    pure (fromNode, weightedNeighbors)

neighborsP :: Parsec String () [(Node, Int)]
neighborsP =
    choice
        [ try (C.string "no other bags") $> []
        , sepBy neighborP (C.string ", ")
        ]

neighborP :: Parsec String () (Node, Int)
neighborP = do
    weight <- integralP
    _ <- C.char ' '
    neighborNode <-
        manyTill anyToken $
            try (C.string " bags")
                <|> try (C.string " bag")
    pure (neighborNode, weight)

integralP :: (Read a, Integral a) => Parsec String () a
integralP = read <$> many1 C.digit
