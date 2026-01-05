module Day25.Main where

import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Ord (Down (..))
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Text.Parsec (Parsec, many, many1, try)
import Text.Parsec.Char qualified as C
import Text.Parsec.String (parseFromFile)

filePath :: FilePath
filePath = "src/Day25/data.txt"

main :: IO ()
main = do
    rows <-
        parseFromFile rowsP filePath
            <&> unwrapEither
    let graph = newGraph rows
    let part1Answer = part1 graph
    putStrLn $ "PART1: " ++ show part1Answer

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Left a) = error $ show a
unwrapEither (Right b) = b

newtype Graph = Graph {adjList :: Map NodeId (Set NodeId)}

type NodeId = String
type Edge = (NodeId, NodeId)

part1 :: Graph -> Int
part1 graph =
    componentSize1 * componentSize2
  where
    componentSize1 = length $ shortestPathsFrom (head allNodes) cutGraph
    componentSize2 = length allNodes - componentSize1
    allNodes = nodes graph
    cutGraph = foldr removeEdge graph cutEdges
    cutEdges =
        take 3
            . sortOn (Down . (edgeCounts M.!))
            $ M.keys edgeCounts
    edgeCounts =
        M.fromListWith
            (+)
            [ (sortPair edge, 1 :: Int)
            | node <- nodes graph
            , path <- shortestPathsFrom node graph
            , let edges = adjacentPairs path
            , edge <- edges
            ]

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs xs = zip xs (drop 1 xs)

sortPair :: (Ord a) => (a, a) -> (a, a)
sortPair (x, y) = (min x y, max x y)

newGraph :: [(NodeId, [NodeId])] -> Graph
newGraph rows =
    Graph
        { adjList =
            M.fromListWith
                S.union
                $ concat
                    [ [ (from, S.singleton to)
                      , (to, S.singleton from)
                      ]
                    | (from, tos) <- rows
                    , to <- tos
                    ]
        }

nodes :: Graph -> [NodeId]
nodes Graph{adjList} = M.keys adjList

neighbors :: NodeId -> Graph -> [NodeId]
neighbors node Graph{adjList} = toList $ adjList M.! node

removeEdge :: Edge -> Graph -> Graph
removeEdge (u, v) Graph{adjList} =
    Graph
        { adjList =
            adjList
                & M.adjust (S.delete v) u
                & M.adjust (S.delete u) v
        }

shortestPathsFrom :: NodeId -> Graph -> [[NodeId]]
shortestPathsFrom startNode graph =
    map toList $
        bfs
            (Seq.singleton (startNode, Seq.empty))
            S.empty
            []
  where
    bfs Empty _ acc = acc
    bfs ((currNode, path) :<| queue) seen acc
        | currNode `S.member` seen = bfs queue seen acc
        | otherwise =
            let path' = path :|> currNode
             in bfs
                    ( queue
                        >< Seq.fromList
                            [ (neighbor, path')
                            | neighbor <- neighbors currNode graph
                            ]
                    )
                    (S.insert currNode seen)
                    (path' : acc)

type Parser = Parsec String ()

rowsP :: Parser [(NodeId, [NodeId])]
rowsP = sepBy1 rowP C.newline

rowP :: Parser (NodeId, [NodeId])
rowP = do
    from <- nodeIdP
    _ <- C.string ": "
    tos <- sepBy1 nodeIdP (C.char ' ')
    pure (from, tos)

nodeIdP :: Parser NodeId
nodeIdP = many1 (try C.letter)

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 aP sepP = do
    a <- aP
    as <- many (try $ sepP >> aP)
    pure (a : as)
