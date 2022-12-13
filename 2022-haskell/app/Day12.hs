module Day12 where

import Data.Char
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List
import Data.Maybe
import Safe

type Coord = (Int, Int)
type Distance = Int
type Matrix a = HashMap Coord a

listsToHashMap :: [[a]] -> HashMap Coord a
listsToHashMap rows =
  foldr
    (uncurry insertRow)
    HashMap.empty
    $ zip rows [0..]
  
insertRow :: [a] -> Int -> HashMap Coord a -> HashMap Coord a
insertRow row i hm =
  foldr
    (\(item, j) hm' -> HashMap.insert (i, j) item hm')
    hm
    $ zip row [0..]
  

filePath :: String
filePath = "data/Day12.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath
  let matrix = listsToHashMap . lines $ inputStr

  -- let part1Answer = part1 matrix
  -- putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 matrix
  putStrLn $ "PART 2: " ++ show part2Answer

startChar :: Char
startChar = 'S'

stopChar :: Char
stopChar = 'E'

-- part1 :: Matrix Char -> Int
-- part1 matrix =
--   let startCoord = findKey matrix 'S'
--       finalAnswer = findMinPathLength matrix 'E' <$> startCoord
--    in fromMaybe 0 finalAnswer

part2 :: Matrix Char -> Int
part2 matrix =
  let startCoord = findKey matrix 'E'
      finalAnswer = findMinPathLength matrix 'a' <$> startCoord
  in fromMaybe 0 finalAnswer

findMinPathLength :: Matrix Char -> Char -> Coord -> Int
findMinPathLength matrix destChar startCoord =
  let dists = doDijkstra matrix startCoord
      destCoords = findKeys matrix destChar
      subDists = HashMap.filterWithKey (\k _ -> elem k destCoords) dists
      distanceVals = HashMap.elems subDists
      finalAnswer = fromMaybe 0 (minimumMay distanceVals)
   in finalAnswer

doDijkstra :: Matrix Char -> Coord -> HashMap Coord Distance
doDijkstra matrix startCoord =
  let visited = HashSet.empty
      unvisited = HashMap.keysSet matrix
      dists = HashMap.map (const maxBound) matrix
      dists' = HashMap.insert startCoord 0 dists
   in dijsktra matrix startCoord visited unvisited dists'

dijsktra :: Matrix Char
  -> Coord
  -> HashSet Coord
  -> HashSet Coord
  -> HashMap Coord Distance
  -> HashMap Coord Distance
dijsktra matrix curr visited unvisited dists = 
  let unvisitedNeighbors =
        filter
          (flip HashSet.member unvisited)
          (getValidNeighbors matrix curr)
      dists' = foldr (updateDists curr) dists unvisitedNeighbors
      visited' = HashSet.insert curr visited
      unvisited' = HashSet.delete curr unvisited
      next = findMinimumCoord dists' unvisited'
   in case (next) of
           (Just next') -> dijsktra matrix next' visited' unvisited' dists'
           _            -> dists'

updateDists :: Coord -> Coord -> HashMap Coord Distance -> HashMap Coord Distance
updateDists curr neighbor dists =
  let currDist = HashMap.findWithDefault maxBound curr dists
      newNeighborDist = if currDist == maxBound then maxBound else currDist + 1
      dists' = HashMap.adjust (min newNeighborDist) neighbor dists
   in dists'

getValidNeighbors :: Matrix Char -> Coord -> [Coord]
getValidNeighbors matrix currCoord =
  filter
    (isValidNeighborHeight matrix currCoord)
    (getNeighbors matrix currCoord)

isValidNeighborHeight :: Matrix Char -> Coord -> Coord -> Bool
isValidNeighborHeight matrix currCoord neighbor =
  let currentChar = HashMap.lookup currCoord matrix
      neighborChar = HashMap.lookup neighbor matrix
   in case (neighborChar, currentChar) of
           (Nothing , _       ) -> False
           (_       , Nothing ) -> False
           (Just (n), Just (c))
               -- PART 2
               | c == startChar -> n >= startChar
               | otherwise      -> n >= prevChar c

             -- PART 1
             -- | n == stopChar -> 'z' <= nextChar c
             -- | otherwise     -> n <= nextChar c

getNeighbors :: Matrix a -> Coord -> [Coord]
getNeighbors matrix (i, j) =
  filter
    (flip HashMap.member matrix)
    $ [
      (i+1, j),
      (i-1, j),
      (i, j+1),
      (i, j-1)
    ]

nextChar :: Char -> Char
nextChar c
  | c == startChar = nextChar 'a'
  | c == stopChar  = nextChar 'z'
  | otherwise      = chr (ord c + 1)

prevChar :: Char -> Char
prevChar c
  | c == startChar = prevChar 'a'
  | c == stopChar  = prevChar 'z'
  | otherwise      = chr (ord c - 1)

findMinimumCoord :: HashMap Coord Distance -> HashSet Coord -> Maybe Coord
findMinimumCoord dists coords
  | HashSet.null coords = Nothing
  | otherwise =
      Just
        . snd
        . minimum
        . HashSet.map (\c -> (,)
                               (HashMap.findWithDefault maxBound c dists)
                               c)
        $ coords

findKey :: (Eq v, Eq k, Hashable k) => HashMap k v -> v -> Maybe k
findKey hm v =
  find
    (\k -> Just v == HashMap.lookup k hm)
    $ HashMap.keys hm

findKeys :: (Eq v, Eq k, Hashable k) => HashMap k v -> v -> [k]
findKeys hm v =
  filter
    (\k -> Just v == HashMap.lookup k hm)
    $ HashMap.keys hm
