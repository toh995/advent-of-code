module Day21.Part01 where

import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Safe

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

neighborCoords :: Matrix a -> Coord -> [Coord]
neighborCoords m (i, j) =
  filter
    (isValidCoord m)
    [ (i + 1, j)
    , (i - 1, j)
    , (i, j + 1)
    , (i, j - 1)
    ]

coordsForVal :: (Eq a) => a -> Matrix a -> [Coord]
coordsForVal a m =
  filter ((== Just a) . lookupM m)
    . HashMap.keys
    . hm
    $ m

-- IO
filePath :: String
filePath = "src/Day21/data.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath
  let matrix =
        buildMatrix
          . lines
          $ inputStr

  let part1Answer = part1 matrix 64
  putStrLn $ "PART 1: " ++ show part1Answer

-- Main logic
part1 :: Matrix Char -> Int -> Int
part1 m numSteps =
  length
    . headDef HashSet.empty
    . drop numSteps
    . iterate (walkAll m)
    $ startCoords
 where
  startCoords = HashSet.fromList $ coordsForVal 'S' m

walkAll :: Matrix Char -> HashSet Coord -> HashSet Coord
walkAll m =
  HashSet.fromList
    . concatMap (walk m)
    . HashSet.toList

walk :: Matrix Char -> Coord -> [Coord]
walk m =
  filter
    ( \coord ->
        let val = lookupM m coord
         in val == Just '.'
              || val == Just 'S'
    )
    . neighborCoords m
