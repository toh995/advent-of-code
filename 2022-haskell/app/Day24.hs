module Day24 where

import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Heap (MinHeap)
import qualified Data.Heap as Heap
import Data.List
import Data.Maybe
import GHC.Utils.Misc hiding (Direction)
import Safe

filePath :: String
filePath = "data/Day24.txt"

main :: IO ()
main = do
  board <- parseBoard . lines <$> readFile filePath

  let part1Answer = part1 board
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 board
  putStrLn $ "PART 2: " ++ show part2Answer

--------------------------
-- SECTION: Parts 1 & 2 --
--------------------------
part1 :: Board -> Int
part1 board =
  let (finalVertex, finalState) = aStarSearch
                                    board
                                    (endVertex board)
                                    (initialState startVertex (endVertex board))
   in getPathLength finalVertex finalState

part2 :: Board -> Maybe Int
part2 board = do
  let (dest1, state1) = aStarSearch
                          board
                          (endVertex board)
                          (initialState startVertex (endVertex board))
  let len1 = getPathLength dest1 state1
  dest1Prev <- getPrevVertex dest1 state1

  let origin2 = dest1 {minute = (+1) <$> minute dest1Prev}
  let board2 = removeVertex (endVertex board) board
  let (dest2, state2) = aStarSearch
                         board2
                         startVertex
                         (initialState origin2 startVertex)
  let len2 = getPathLength dest2 state2
  dest2Prev <- getPrevVertex dest2 state2

  let origin3 = dest2 {minute = (+1) <$> minute dest2Prev}
  let board3 = removeVertex startVertex board
  let (dest3, state3) = aStarSearch
                          board3
                          (endVertex board)
                          (initialState origin3 (endVertex board))
  let len3 = getPathLength dest3 state3

  return $ len1 + len2 + len3

----------------------
-- SECTION: Parsing --
----------------------
type Line = String

parseBoard :: [Line] -> Board
parseBoard ls =
  let cleaned = cleanLines ls
      numRows = length cleaned
      numCols = length $ headDef [] cleaned
      maxMinutes = lcm numRows numCols
   in Board {
          numRows
        , numCols
        , maxMinutes
        , coordToVertices = buildCoordToVertices numRows numCols maxMinutes cleaned
      }

cleanLines :: [Line] -> [Line]
cleanLines = map removeFirstAndLast . removeFirstAndLast

getBlizzards :: [Line] -> [Blizzard]
getBlizzards ls =
  let linesAndIs = zip ls [0..]
   in concatMap (uncurry getBlizzardsForRow) linesAndIs

getCoords :: I -> J -> [Coord]
getCoords numRows numCols =
  (,) <$> [0..numRows - 1] <*> [0..numCols - 1]

getBlizzardsForRow :: [Char] -> I -> [Blizzard]
getBlizzardsForRow chars i =
  let charsAndJs = zip chars [0..]
   in mapMaybe
        (\(c, j) -> Blizzard (i, j) <$> charToDirection c)
        charsAndJs

charToDirection :: Char -> Maybe Direction
charToDirection '^' = Just North
charToDirection 'v' = Just South
charToDirection '>' = Just East
charToDirection '<' = Just West
charToDirection _   = Nothing

buildCoordToVertices :: I -> J -> Minute -> [Line] -> HashMap Coord (HashSet Vertex)
buildCoordToVertices numRows numCols maxMinutes ls =
  let coordToMinutes = buildCoordToMinutes numRows numCols maxMinutes ls
      ret = HashMap.mapWithKey
              (\coord minutes -> HashSet.map (\m -> Vertex coord (Just m)) minutes)
              coordToMinutes
      ret' = HashMap.insertWith HashSet.union (-1,0) (HashSet.singleton $ Vertex (-1,0) Nothing)
               . HashMap.insertWith HashSet.union (numRows, numCols-1) (HashSet.singleton $ Vertex (numRows, numCols-1) Nothing)
               $ ret
   in ret'

buildCoordToMinutes :: I -> J -> Minute -> [Line] -> HashMap Coord (HashSet Minute)
buildCoordToMinutes numRows numCols maxMinutes ls =
  let minuteToOpenSpaces = buildMinuteToOpenSpaces numRows numCols maxMinutes ls
      minCoordPairs = concatMap (\(minute, coords) -> (,) minute <$> coords) $ HashMap.toList minuteToOpenSpaces
      coordToMinutes = foldr
                         (\(minute, coord) ->
                           HashMap.insertWith
                             HashSet.union
                             coord
                             (HashSet.singleton minute))
                         HashMap.empty
                         minCoordPairs
   in coordToMinutes

buildMinuteToOpenSpaces :: I -> J -> Minute -> [Line] -> HashMap Minute [Coord]
buildMinuteToOpenSpaces numRows numCols maxMinutes ls =
  let minuteToBlizzards = buildMinuteToBlizzards numRows numCols maxMinutes ls
      blizzardCoords = HashMap.map (map blizzardCoord) minuteToBlizzards
      allCoords = getCoords numRows numCols
   in HashMap.map (\cs -> allCoords \\ cs) blizzardCoords

buildMinuteToBlizzards :: I -> J -> Minute -> [Line] -> HashMap Minute [Blizzard]
buildMinuteToBlizzards numRows numCols maxMinutes ls =
  finalHm
  where
    (finalHm, _, _) = nTimes maxMinutes f initialTuple
    initialTuple = (HashMap.empty, 0, getBlizzards ls)

    f ::
      (HashMap Minute [Blizzard], Minute, [Blizzard])
      -> (HashMap Minute [Blizzard], Minute, [Blizzard])
    f (hm, currMin, blizzards) =
      let hm' = HashMap.insert currMin blizzards hm
          currMin' = currMin + 1
          blizzards' = map (moveBlizzard numRows numCols) blizzards
       in (hm', currMin', blizzards')

-------------------------
-- SECTION: Core Logic --
-------------------------
type I = Int
type J = Int
type Coord = (I, J)

type Minute = Int

data Board = Board {
    numRows :: Int
  , numCols :: Int
  , maxMinutes :: Minute
  , coordToVertices :: HashMap Coord (HashSet Vertex)
}

data Direction = North | South | East | West
  deriving (Enum, Eq, Show)

data Blizzard = Blizzard {
    blizzardCoord :: Coord
  , blizzardDirection :: Direction
}
  deriving (Eq, Show)

instance Ord Blizzard where
  (<=) (Blizzard coord1 dir1) (Blizzard coord2 dir2)
    | coord1 /= coord2 = coord1 <= coord2
    | otherwise = fromEnum dir1 <= fromEnum dir2

data Vertex = Vertex {
    coord :: Coord
  , minute :: Maybe Minute
}
  deriving (Eq, Show)

instance Ord Vertex where
  compare (Vertex coord1 _) (Vertex coord2 _) = compare coord1 coord2

instance Hashable Vertex where
  hashWithSalt n (Vertex coord minute) = hashWithSalt n (coord, minute)
  hash (Vertex coord minute) = hash (coord, minute)

endVertex :: Board -> Vertex
endVertex (Board {numRows, numCols}) =
  Vertex (numRows, numCols-1) Nothing

endCoord :: Board -> Coord
endCoord (Board {numRows, numCols}) =
  (numRows, numCols-1)

startVertex :: Vertex
startVertex = Vertex (-1, 0) Nothing

startCoord :: Coord
startCoord = (-1, 0)

moveBlizzard :: I -> J -> Blizzard -> Blizzard
moveBlizzard numRows _ (Blizzard (i, j) North) = Blizzard ((i-1) `mod` numRows, j) North
moveBlizzard numRows _ (Blizzard (i, j) South) = Blizzard ((i+1) `mod` numRows, j) South
moveBlizzard _ numCols (Blizzard (i, j) East)  = Blizzard (i, (j+1) `mod` numCols) East
moveBlizzard _ numCols (Blizzard (i, j) West)  = Blizzard (i, (j-1) `mod` numCols) West

removeVertex :: Vertex -> Board -> Board
removeVertex v@(Vertex{coord}) board@(Board {coordToVertices}) =
  board {
    coordToVertices = HashMap.adjust (HashSet.delete v) coord coordToVertices
  }

getNeighbors :: Board -> Vertex -> HashSet Vertex
getNeighbors board@(Board {coordToVertices, maxMinutes}) (Vertex (i,j) currMin) =
  let neighborCoords = [(i, j), (i-1, j), (i+1, j), (i, j-1), (i, j+1)]
      vertexSets = map
                     (flip (HashMap.findWithDefault HashSet.empty) coordToVertices)
                     neighborCoords
      targetMin = mod <$> ((+1) <$> currMin) <*> pure maxMinutes
      allVertices = HashSet.unions vertexSets
      allVertices' = if (i,j) == startCoord || (i,j) == endCoord board
                       then allVertices
                       else HashSet.filter
                              (\v -> isNothing currMin || isNothing (minute v) || minute v == targetMin)
                              allVertices
   in allVertices'

edgeWeight :: Board -> Vertex -> Vertex -> Maybe Int
edgeWeight board from to
  | manhattanDistance (coord from) (coord to) > 1 = Nothing
  | otherwise =
      case (minute from, minute to) of
        (Nothing, (Just m)) -> Just m
        (_, Nothing) -> Just 1
        (Just fromMin, Just toMin) ->
          if manhattanDistance (coord from) (coord to) == 1 && fromMin == toMin
          then Just $ maxMinutes board
          else Just $ (toMin - fromMin) `mod` (maxMinutes board)

----------------------------------
-- SECTION: A* Search Algorithm --
----------------------------------
data AStarState = AStarState {
    gScore :: HashMap Vertex Int
  , fScore :: HashMap Vertex Int
  , openSet :: MinHeap (Int, Vertex)
  , cameFrom :: HashMap Vertex Vertex
}

getPathLength :: Vertex -> AStarState -> Int
getPathLength vertex (AStarState {fScore}) =
  fromMaybe 0 $ HashMap.lookup vertex fScore

getPrevVertex :: Vertex -> AStarState -> Maybe Vertex
getPrevVertex vertex (AStarState {cameFrom}) =
  HashMap.lookup vertex cameFrom

initialState :: Vertex -> Vertex -> AStarState
initialState originVertex destVertex =
  AStarState {
      gScore = HashMap.singleton originVertex 0
    , fScore = HashMap.singleton originVertex startFScore
    , openSet = Heap.singleton (startFScore, originVertex)
    , cameFrom = HashMap.empty
  }
  where
    startFScore = h originVertex destVertex

h :: Vertex -> Vertex -> Int
h (Vertex source _) (Vertex goal _) = manhattanDistance source goal

aStarSearch ::
  Board
  -> Vertex
  -> AStarState
  -> (Vertex, AStarState)
aStarSearch board goal state@(AStarState{openSet})
  | current == goal = (current, state)
  | otherwise = 
      let state' = state{openSet = Heap.drop 1 openSet}
          neighbors = getNeighbors board current
          state'' = foldr (newStateForNeighbor board goal current) state' neighbors
       in aStarSearch board goal state''
  where
    current = snd . fromJust $ Heap.viewHead openSet

newStateForNeighbor :: Board -> Vertex -> Vertex -> Vertex -> AStarState -> AStarState
newStateForNeighbor board goal current neighbor state@(AStarState {gScore, fScore, openSet, cameFrom})
  | not shouldUpdate = state
  | otherwise =
      state {
          gScore = HashMap.insert neighbor (fromJust newGScore) gScore
        , fScore = HashMap.insert neighbor (fromJust newGScore + h neighbor goal) fScore
        , openSet = Heap.insert ((fromJust newGScore + h neighbor goal), neighbor) openSet
        , cameFrom = HashMap.insert neighbor current cameFrom
      }
  where
    currGScore = HashMap.lookup neighbor gScore
    newGScore = (+) <$> HashMap.lookup current gScore <*> edgeWeight board current neighbor
    shouldUpdate =
      case (newGScore, currGScore) of
        (Nothing , _        ) -> False
        (Just _  , Nothing  ) -> True
        (Just new, Just curr) -> new < curr

--------------------
-- SECTION: Utils --
--------------------
removeFirstAndLast :: [a] -> [a]
removeFirstAndLast = removeFirst . removeLast

removeFirst :: [a] -> [a]
removeFirst [] = []
removeFirst (_:xs) = xs

removeLast :: [a] -> [a]
removeLast = reverse . removeFirst . reverse

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (i, j) (i', j') = abs (i'-i) + abs (j'-j)
