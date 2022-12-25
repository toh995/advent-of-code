module Day16 where

import Control.Applicative
import Data.Char 
import Data.Either
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List
import Data.Maybe
import Data.Void
import Safe.Foldable
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


----------------------
-- SECTION: Main IO --
----------------------
filePath :: String
filePath = "data/Day16.txt"

main :: IO ()
main = do
  ls <- lines <$> readFile filePath
  let vertices = parseMainVertices ls
  let edges = parseEdges ls
  let flowRateMap = parseFlowRateMap ls

  let graph = buildGraph vertices edges flowRateMap

  let part1Answer = part1 graph
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 graph
  putStrLn $ "PART 2: " ++ show part2Answer

----------------------
-- SECTION: Parsing --
----------------------
type Line = String

type Parser a = Parsec Void String a

parseMainVertices :: [Line] -> HashSet Vertex
parseMainVertices = HashSet.fromList . rights . map (runParser mainVertexParser "")

parseEdges :: [Line] -> HashSet Edge
parseEdges =
  HashSet.fromList
    . concat
    . rights
    . map parseEdges'
  where
    parseEdges' line =
      let mainVertex = singleton <$> runParser mainVertexParser "" line
          adjacentVertices = runParser adjacentVerticesParser "" line
       in liftA2 (liftA2 (,)) mainVertex adjacentVertices

parseFlowRateMap :: [Line] -> HashMap Vertex FlowRate
parseFlowRateMap =
  HashMap.fromList
    . rights
    . map parsePairs
  where
    parsePairs line =
      let vertex = runParser mainVertexParser "" line
          flowRate = runParser flowRateParser "" line
       in (,) <$> vertex <*> flowRate

vertexParser :: Parser Vertex
vertexParser = count 2 $ satisfy isUpper

mainVertexParser :: Parser Vertex
mainVertexParser = string "Valve " *> vertexParser <* space

flowRateParser :: Parser FlowRate
flowRateParser = mainVertexParser *> string "has flow rate=" *> L.decimal

adjacentVerticesParser :: Parser [Vertex]
adjacentVerticesParser =
  flowRateParser
    *> string "; tunnels lead to valves "
    *> vertexParser `sepBy` (string ", ")

-------------------------
-- SECTION: Main Logic --
-------------------------
part1 :: Graph -> Pressure
part1 graph =
  maxPressure graph startVertex totalMins
  where
    startVertex = "AA"
    totalMins = 30

part2 :: Graph -> Pressure
part2 graph =
  let vertices = getNonEmptyVertices graph
      partitioned = partitions . HashSet.toList $ vertices
      startVertex = "AA"
      totalMins = 26
      startPressure = 0
      f (s1, s2) = max1 + max2
        where
          max1 = maximumBound 0 $ HashSet.map (maxPressureUsingVertices graph startPressure totalMins s1 startVertex) s1
          max2 = maximumBound 0 $ HashSet.map (maxPressureUsingVertices graph startPressure totalMins s2 startVertex) s2
   in maximumBound 0 $ map f partitioned

maxPressure :: Graph -> Vertex -> Minutes -> Pressure
maxPressure graph startVertex totalMins =
  let vertices = getNonEmptyVertices graph
      startPressure = 0
   in maximumBound 0 $
        HashSet.map
          (maxPressureUsingVertices graph startPressure totalMins vertices startVertex)
          vertices

maxPressureUsingVertices ::
  Graph
  -> Pressure
  -> Minutes
  -> HashSet Vertex
  -> Vertex
  -> Vertex
  -> Pressure
maxPressureUsingVertices graph pressure minutesLeft unvisitedVertices prevVertex nextVertex 
  | minuteCost <= 1  = pressure
  | flowRate <= 0    = pressure
  | minutesLeft' <= 0 = pressure
  | otherwise =
      maximumBound
        pressure'
        (HashSet.map
          (maxPressureUsingVertices graph pressure' minutesLeft' unvisitedVertices' nextVertex)
          unvisitedVertices')
  where
    minuteCost = fromMaybe 0 $ (+1) <$> getMinDistance graph prevVertex nextVertex
    minutesLeft' = minutesLeft - minuteCost
    flowRate = fromMaybe 0 $ getFlowRate graph nextVertex
    pressure' = pressure + (minutesLeft' * flowRate)
    unvisitedVertices' =
      HashSet.delete nextVertex
        . HashSet.delete prevVertex
        $ unvisitedVertices

-- Divide a list into partitions
-- partitions [1,2] == [({}, {1,2}), ({1}, {2})]
partitions :: Hashable a => [a] -> [(HashSet a, HashSet a)]
partitions [] = []
partitions [x] = [(HashSet.singleton x, HashSet.empty)]
partitions (x:xs) =
  concatMap
    (\(s1, s2) -> [(HashSet.insert x s1, s2), (s1, HashSet.insert x s2)])
    (partitions xs)

----------------------------------------
-- SECTION: Graph Definition, Helpers --
----------------------------------------
type Vertex = String
type Edge = (Vertex, Vertex)
type Distance = Integer
type FlowRate = Integer
type Pressure = Integer
type Minutes = Integer
type MinDistanceMap = HashMap (Vertex, Vertex) Distance

data Graph = Graph {
    vertices :: HashSet Vertex
  , edges :: HashSet Edge
  , minDistanceMap :: MinDistanceMap
  , flowRateMap :: HashMap Vertex FlowRate
}
  deriving (Show)

getMinDistance :: Graph -> Vertex -> Vertex -> Maybe Distance
getMinDistance (Graph {minDistanceMap}) u v =
  HashMap.lookup (u, v) minDistanceMap

getFlowRate :: Graph -> Vertex -> Maybe FlowRate
getFlowRate (Graph {flowRateMap}) v =
  HashMap.lookup v flowRateMap

getNonEmptyVertices :: Graph -> HashSet Vertex
getNonEmptyVertices (Graph {flowRateMap}) =
  HashMap.keysSet
    . HashMap.filter (> 0)
    $ flowRateMap

buildGraph :: HashSet Vertex -> HashSet Edge -> HashMap Vertex FlowRate -> Graph
buildGraph vertices edges flowRateMap =
  Graph {
     vertices
    , edges
    , flowRateMap = flowRateMap
    , minDistanceMap = buildMinDistanceMap vertices edges
  }

-- Inspired by https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
buildMinDistanceMap :: HashSet Vertex -> HashSet Edge -> MinDistanceMap
buildMinDistanceMap vertices edges =
  insertKs vertices
    . insertVertices vertices
    . insertEdges edges
    $ HashMap.empty

insertEdges :: HashSet Edge -> MinDistanceMap -> MinDistanceMap
insertEdges edges hm =
  foldr f hm edges
  where
    f (u, v) hm' =
      HashMap.insert (u, v) 1 
        . HashMap.insert (v, u) 1
        $ hm'

insertVertices :: HashSet Vertex -> MinDistanceMap -> MinDistanceMap
insertVertices vertices hm =
  foldr f hm vertices
  where
    f v hm' = HashMap.insert (v, v) 0 hm'

insertKs :: HashSet Vertex -> MinDistanceMap -> MinDistanceMap
insertKs vertices hm =
  foldr (insertK vertices) hm vertices

insertK :: HashSet Vertex -> Vertex -> MinDistanceMap -> MinDistanceMap
insertK vertices k hm =
  foldr f hm vertexPairs
  where
    vertices' = HashSet.toList vertices
    vertexPairs = [(i,j) | i <- vertices',  j <- vertices']
    f (i,j) hm' =
      let currDist = HashMap.lookup (i,j) hm'
          iToK = HashMap.lookup (i,k) hm'
          kToJ = HashMap.lookup (k,j) hm'
          otherDist = (+) <$> iToK <*> kToJ
          newDist = (min <$> currDist <*> otherDist)
              <|> currDist
              <|> otherDist
          hm'' = HashMap.insert (i,j) <$> newDist <*> pure hm'
       in fromMaybe hm' hm''
