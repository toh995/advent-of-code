module Day16.Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Functor
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable
import GHC.Generics (Generic)

-- Matrix Primitives
type I = Int
type J = Int
type Coord = (I, J)

data Matrix a = Matrix
  { iMax, jMax :: Int
  , hm :: HashMap Coord a
  }
  deriving (Eq, Show)

type Tile = Char

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

isValidCoord :: Matrix a -> Coord -> Bool
isValidCoord Matrix{iMax, jMax} (i, j) =
  (0 <= i && i <= iMax)
    && (0 <= j && j <= jMax)

lookupM :: Matrix a -> Coord -> Maybe a
lookupM (Matrix{hm}) =
  flip HashMap.lookup hm

-- Main IO
filePath :: String
filePath = "src/Day16/data.txt"

main :: IO ()
main = do
  matrix <- readFile filePath <&> buildMatrix . lines

  let part1Answer = part1 matrix
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 matrix
  putStrLn $ "PART 2: " ++ show part2Answer

-- Core logic
data Direction = R | L | U | D
  deriving (Eq, Generic)

instance Hashable Direction

type Position = (Coord, Direction)

part1 :: Matrix Tile -> Maybe Int
part1 m =
  getTotalEnergy m initialP
 where
  initialP = ((0, 0), R)

part2 :: Matrix Tile -> Maybe Int
part2 m =
  maximum
    . map (getTotalEnergy m)
    $ getInitialPs m

getInitialPs :: Matrix a -> [Position]
getInitialPs Matrix{iMax, jMax} =
  map (\j -> ((0, j), D)) [0 .. jMax]
    ++ map (\j -> ((iMax, j), U)) [0 .. jMax]
    ++ map (\i -> ((i, 0), R)) [0 .. iMax]
    ++ map (\i -> ((i, jMax), L)) [0 .. iMax]

getTotalEnergy :: Matrix Tile -> Position -> Maybe Int
getTotalEnergy matrix initialP = do
  seen <-
    execStateT
      (fooS matrix initialP)
      HashSet.empty
  pure (length . HashSet.map fst $ seen)

fooS :: Matrix Tile -> Position -> StateT (HashSet Position) Maybe ()
fooS matrix pos = do
  seen <- get
  put $ HashSet.insert pos seen
  nextPoses <- lift $ getNextPoses matrix pos
  let nextPoses' =
        filter
          (not . (`HashSet.member` seen))
          nextPoses
  mapM_ (fooS matrix) nextPoses'

getNextPoses :: Matrix Tile -> Position -> Maybe [Position]
getNextPoses matrix ((i, j), dir) = do
  tile <- lookupM matrix (i, j)
  let nextPoses =
        case dir of
          R ->
            if
              | tile == '.' -> [((i, j + 1), R)]
              | tile == '/' -> [((i - 1, j), U)]
              | tile == '\\' -> [((i + 1, j), D)]
              | tile == '-' -> [((i, j + 1), R)]
              | tile == '|' -> [((i - 1, j), U), ((i + 1, j), D)]
              | otherwise -> []
          L ->
            if
              | tile == '.' -> [((i, j - 1), L)]
              | tile == '/' -> [((i + 1, j), D)]
              | tile == '\\' -> [((i - 1, j), U)]
              | tile == '-' -> [((i, j - 1), L)]
              | tile == '|' -> [((i - 1, j), U), ((i + 1, j), D)]
              | otherwise -> []
          U ->
            if
              | tile == '.' -> [((i - 1, j), U)]
              | tile == '/' -> [((i, j + 1), R)]
              | tile == '\\' -> [((i, j - 1), L)]
              | tile == '-' -> [((i, j + 1), R), ((i, j - 1), L)]
              | tile == '|' -> [((i - 1, j), U)]
              | otherwise -> []
          D ->
            if
              | tile == '.' -> [((i + 1, j), D)]
              | tile == '/' -> [((i, j - 1), L)]
              | tile == '\\' -> [((i, j + 1), R)]
              | tile == '-' -> [((i, j + 1), R), ((i, j - 1), L)]
              | tile == '|' -> [((i + 1, j), D)]
              | otherwise -> []
  pure $ filter (isValidCoord matrix . fst) nextPoses
