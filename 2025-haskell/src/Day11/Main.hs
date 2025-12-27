{-# LANGUAGE NamedFieldPuns #-}

module Day11.Main where

import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec (Parsec, many, many1, try)
import qualified Text.Parsec.Char as C
import Text.Parsec.String (parseFromFile)

filePath :: FilePath
filePath = "src/Day11/data.txt"

main :: IO ()
main = do
    graph <-
        parseFromFile graphP filePath
            <&> unwrapEither
    let part1Answer = part1 graph
    putStrLn $ "PART1: " ++ show part1Answer
    let part2Answer = part2 graph
    putStrLn $ "PART2: " ++ show part2Answer

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Left a) = error $ show a
unwrapEither (Right b) = b

part1 :: Graph -> Int
part1 graph =
    let source = "you"
        dest = "out"
     in pathCounts dest graph M.! source

part2 :: Graph -> Int
part2 graph =
    let toDAC = pathCounts "dac" graph
        toFFT = pathCounts "fft" graph
        toOUT = pathCounts "out" graph
     in -- svr -> dac -> fft -> out
        product
            [ toDAC M.! "svr"
            , toFFT M.! "dac"
            , toOUT M.! "fft"
            ]
            +
            -- svr -> fft -> dac -> out
            product
                [ toFFT M.! "svr"
                , toDAC M.! "fft"
                , toOUT M.! "dac"
                ]

type NodeId = String

newtype Graph = Graph
    { adjList :: Map NodeId (Set NodeId)
    }

allNodes :: Graph -> Set NodeId
allNodes Graph{adjList} =
    S.fromList $
        concat
            [ from : toList tos
            | (from, tos) <- M.assocs adjList
            ]

neighbors :: NodeId -> Graph -> Set NodeId
neighbors node Graph{adjList} = M.findWithDefault S.empty node adjList

pathCounts :: NodeId -> Graph -> Map NodeId Int
pathCounts dest graph = cached
  where
    cached =
        M.fromList
            [ (node, count node)
            | node <- toList $ allNodes graph
            ]
    count node
        | node == dest = 1
        | otherwise =
            sum
                [ cached M.! neighbor
                | neighbor <- toList $ neighbors node graph
                ]

type Parser = Parsec String ()

graphP :: Parser Graph
graphP = do
    rows <- sepBy1 rowP C.newline
    let adjList = M.fromListWith S.union rows
    pure Graph{adjList}

rowP :: Parser (NodeId, Set NodeId)
rowP = do
    from <- nodeIdP
    _ <- C.string ": "
    tos <- sepBy1 nodeIdP (C.char ' ')
    pure (from, S.fromList tos)

nodeIdP :: Parser NodeId
nodeIdP = many1 (try C.letter)

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 aP sepP = do
    a <- aP
    as <- many (try $ sepP >> aP)
    pure (a : as)
