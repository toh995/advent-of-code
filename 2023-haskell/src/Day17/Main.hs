module Day17.Main where

import Algorithm.Search
import Data.Char
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.Maybe

-- Matrix Primitives
type I = Int
type J = Int
type Coord = (I, J)

data Matrix a = Matrix
  { iMax, jMax :: Int
  , hm :: HashMap Coord a
  }
  deriving (Eq, Show)

buildMatrix :: [[a]] -> Matrix a
buildMatrix [] = error "buildMatrix: empty list."
buildMatrix rows@(r : _) =
  Matrix
    { iMax = length rows - 1
    , jMax = length r - 1
    , hm =
        foldr
          insertRow
          HashMap.empty
          (zip rows [0 ..])
    }
 where
  insertRow (row, i) hm =
    foldr
      (\(x, j) hm' -> HashMap.insert (i, j) x hm')
      hm
      (zip row [0 ..])

lookupM :: Matrix a -> Coord -> Maybe a
lookupM (Matrix{hm}) =
  flip HashMap.lookup hm

isValidCoord :: Matrix a -> Coord -> Bool
isValidCoord Matrix{iMax, jMax} (i, j) =
  (0 <= i && i <= iMax)
    && (0 <= j && j <= jMax)

-- Main
filePath :: String
filePath = "src/Day17/data.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath
  let matrix =
        buildMatrix
          . map (map digitToInt)
          . lines
          $ inputStr

  let part1Answer = part1 matrix
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 matrix
  putStrLn $ "PART 2: " ++ show part2Answer

-- Core Path-finding logic
data Direction = U | D | L | R
  deriving (Eq, Ord, Show)

type Node = (Coord, Direction)

part1 :: Matrix Int -> Int
part1 matrix =
  let initialNodes = [((0, 0), D), ((0, 0), L)]
      costs =
        map fst
          . mapMaybe
            ( dijkstra
                (getNeighbors1 matrix)
                (getCost matrix)
                (isTerminal matrix)
            )
          $ initialNodes
   in minimum costs

part2 :: Matrix Int -> Int
part2 matrix =
  let initialNodes = [((0, 0), D), ((0, 0), L)]
      costs =
        map fst
          . mapMaybe
            ( dijkstra
                (getNeighbors2 matrix)
                (getCost matrix)
                (isTerminal matrix)
            )
          $ initialNodes
   in minimum costs

getNeighbors1 :: Matrix a -> Node -> [Node]
getNeighbors1 _ ((0, 0), D) = [((0, 1), R), ((0, 2), R)]
getNeighbors1 _ ((0, 0), L) = [((1, 0), D), ((2, 0), D)]
getNeighbors1 matrix ((i, j), dir) =
  let allNeighbors =
        if dir `elem` [U, D]
          then
            [((i, j - 1), L), ((i, j - 2), L), ((i, j - 3), L)]
              ++ [((i, j + 1), R), ((i, j + 2), R), ((i, j + 3), R)]
          else
            [((i - 1, j), U), ((i - 2, j), U), ((i - 3, j), U)]
              ++ [((i + 1, j), D), ((i + 2, j), D), ((i + 3, j), D)]
   in filter
        (isValidCoord matrix . fst)
        allNeighbors

getNeighbors2 :: Matrix a -> Node -> [Node]
getNeighbors2 _ ((0, 0), D) = map (\delta -> ((0, delta), R)) [3 .. 9]
getNeighbors2 _ ((0, 0), L) = map (\delta -> ((delta, 0), D)) [3 .. 9]
getNeighbors2 matrix ((i, j), dir) =
  let allNeighbors =
        if dir `elem` [U, D]
          then
            map (\delta -> ((i, j - delta), L)) [4 .. 10]
              ++ map (\delta -> ((i, j + delta), R)) [4 .. 10]
          else
            map (\delta -> ((i - delta, j), U)) [4 .. 10]
              ++ map (\delta -> ((i + delta, j), D)) [4 .. 10]
   in filter
        (isValidCoord matrix . fst)
        allNeighbors

getCost :: (Num a) => Matrix a -> Node -> Node -> a
getCost matrix ((i, j), _) ((i', j'), _) =
  let coords =
        if
          | i == i' && j == j' -> []
          | i == i' && j < j' -> (i,) <$> [j + 1 .. j']
          | i == i' && j > j' -> (i,) <$> [j - 1, j - 2 .. j']
          | i < i' && j == j' -> (,j) <$> [i + 1 .. i']
          | i > i' && j == j' -> (,j) <$> [i - 1, i - 2 .. i']
          | otherwise -> []
      costs = sum . mapMaybe (lookupM matrix) $ coords
   in costs

isTerminal :: Matrix a -> Node -> Bool
isTerminal (Matrix{iMax, jMax}) ((i, j), _) =
  (i, j) == (iMax, jMax)
