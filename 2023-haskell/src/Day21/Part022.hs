module Day21.Part022 where

-- INSPIRED BY https://www.reddit.com/r/adventofcode/comments/18nevo3/comment/kebnr7e/?utm_source=share&utm_medium=web2x&context=3

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import Data.Foldable
import Data.Function.Memoize
import Data.Functor
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Debug.Trace
import Safe (headMay)

-- Matrix Primitives
type I = Int
type J = Int
type Coord = (I, J)

data Matrix a = Matrix
  { len :: Int
  , hm :: HashMap Coord a
  }
  deriving (Eq, Show)

buildMatrix :: [[a]] -> Matrix a
buildMatrix [] = error "buildMatrix: empty list."
buildMatrix rows =
  Matrix
    { len = length rows
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
lookupM (Matrix{hm, len}) (i, j) =
  HashMap.lookup (i', j') hm
 where
  i' = i `mod` len
  j' = j `mod` len

neighborCoords :: Coord -> [Coord]
neighborCoords (i, j) =
  [ (i + 1, j)
  , (i - 1, j)
  , (i, j + 1)
  , (i, j - 1)
  ]

neighborCoordsForVals :: (Eq a) => Matrix a -> [a] -> Coord -> [Coord]
neighborCoordsForVals m vals =
  filter
    ( \coord ->
        Just True
          == (lookupM m coord <&> (`elem` vals))
    )
    . neighborCoords

coordForVal :: (Eq a) => a -> Matrix a -> Maybe Coord
coordForVal a (Matrix{hm}) =
  headMay
    . map fst
    . filter ((== a) . snd)
    . HashMap.toList
    $ hm

countVals :: (Eq a) => a -> Matrix a -> Int
countVals a (Matrix{hm}) =
  length
    . filter (== a)
    . HashMap.elems
    $ hm

-- Main IO
filePath :: String
filePath = "src/Day21/data.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath
  let matrix =
        buildMatrix
          . lines
          $ inputStr
  let totalSteps = 26501365

  let part2Answer = part2 matrix totalSteps
  putStrLn $ "PART 2: " ++ show part2Answer

-- Core Logic
part2 :: Matrix Char -> Int -> Maybe Int
part2 matrix totalSteps = do
  (iStart, jStart) <- coordForVal 'S' matrix
  let gridLen = len matrix
  let validators =
        [ \(i, j) -> iStart <= i && i < iStart + gridLen && jStart <= j && j < jStart + gridLen
        , \(i, j) -> iStart - gridLen < i && i <= iStart && jStart <= j && j < jStart + gridLen
        , \(i, j) -> iStart - gridLen < i && i <= iStart && jStart - gridLen < j && j <= jStart
        , \(i, j) -> iStart <= i && i < iStart + gridLen && jStart - gridLen < j && j <= jStart
        ]
  let innerCount =
        sum $
          map
            (countSteps matrix totalSteps (iStart, jStart))
            validators
  let axesCount = 4 * countAxis totalSteps
  pure $ innerCount - axesCount

countAxis :: Int -> Int
countAxis totalSteps =
  -- Can prove this via induction
  (totalSteps `div` 2) + 1

countSteps :: Matrix Char -> Int -> Coord -> (Coord -> Bool) -> Int
countSteps matrix totalSteps startCoord isValidCoord =
  evalState
    (countStepsS matrix totalSteps isValidCoord)
    initialS
 where
  initialS =
    S
      { seen = mempty
      , currCoords = [startCoord]
      , distance = 0
      }

data S = S
  { seen :: HashSet Coord
  , currCoords :: [Coord]
  , distance :: Int
  }

countStepsS :: Matrix Char -> Int -> (Coord -> Bool) -> State S Int
countStepsS matrix totalSteps isValidCoord = do
  s <- get
  let currCount =
        length (currCoords s)
          * numReachableCells (len matrix) totalSteps (distance s)

  if null $ currCoords s
    then pure 0
    else
      (+ currCount) <$> do
        put
          S
            { seen = seen s <> HashSet.fromList (currCoords s)
            , currCoords =
                filter isValidCoord
                  . HashSet.toList
                  . (`HashSet.difference` seen s)
                  . HashSet.fromList
                  . concatMap (neighborCoordsForVals matrix ['.'])
                  $ currCoords s
            , distance = distance s + 1
            }
        countStepsS matrix totalSteps isValidCoord

numReachableCells :: Int -> Int -> Int -> Int
numReachableCells gridLen totalSteps distance =
  let maxGridAmt = (totalSteps - distance) `div` gridLen
      targetParity = totalSteps `mod` 2 -- odd
      distParity = distance `mod` 2 -- even
      gridParity = (targetParity - distParity) `mod` 2 -- odd
      gridAmts = [gridParity, gridParity + 2 .. maxGridAmt]
   in sum . map numPairs $ gridAmts

-- Compute the # of pairs (a, b) such that a,b >= 0, and a + b = n
-- See explanation below
numPairs :: Int -> Int
numPairs n
  | n < 0 = 0
  | n == 0 = 1
  | otherwise = n + 1

{-
Explanation:
For n > 0

If n is odd:
  We have n//2 + 1 pairs which satisfy a + b = n:
    (0, n), (1, n-1), (2, n-2), ..., (n//2, n//2 + 1)
  For each such pair (a, b), we can also swap the order to (b, a).
  So the total number of pairs is:
    2*(n//2 + 1) = 2*(n//2) + 2 = (n-1) + 2 = n + 1

If n is even:
  We have n/2 + 1 pairs which satisfy a + b = n:
    (0, n), (1, n-1), (2, n-2), ..., (n/2, n/2)

  The pair (n/2, n/2) has exactly one permutation, so this pair
  contributes 1 to the total.

  For the other n/2 pairs, we have two permutations, (a,b) and (b,a).
  So this contributes 2*(n/2) = n to the total.

  So the grand total is n + 1.
-}
