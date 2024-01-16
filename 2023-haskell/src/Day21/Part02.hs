module Day21.Part02 where

-- INSPIRED BY https://www.reddit.com/r/adventofcode/comments/18nevo3/comment/kebnr7e/?utm_source=share&utm_medium=web2x&context=3

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import Data.Function.Memoize
import Data.Functor
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Debug.Trace

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

neighborCoordsForVals :: (Eq a) => Matrix a -> [a] -> Coord -> [Coord]
neighborCoordsForVals m vals =
  filter
    ( \coord ->
        Just True
          == (lookupM m coord <&> (`elem` vals))
    )
    . neighborCoords m

coordsForVal :: (Eq a) => a -> Matrix a -> [Coord]
coordsForVal a m =
  filter ((== Just a) . lookupM m)
    . HashMap.keys
    . hm
    $ m

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
part2 :: Matrix Char -> Int -> Int
part2 matrix totalSteps =
  evalState
    (countStepsS matrix gridLen totalSteps)
    (initialS matrix)
 where
  gridLen = iMax matrix + 1

data S = S
  { seen :: HashSet Coord
  , currCoords :: [Coord]
  , distance :: Int
  }

countStepsS :: Matrix Char -> Int -> Int -> State S Int
countStepsS matrix gridLen totalSteps = do
  s <- get
  let currCount =
        length (currCoords s)
          * numReachableCells gridLen totalSteps (distance s)

  if null $ currCoords s
    then pure 0
    else
      (+ currCount) <$> do
        put
          S
            { seen = seen s <> HashSet.fromList (currCoords s)
            , currCoords =
                HashSet.toList
                  . (`HashSet.difference` seen s)
                  . HashSet.fromList
                  . concatMap (neighborCoordsForVals matrix ['.'])
                  $ currCoords s
            , distance = distance s + 1
            }
        countStepsS matrix gridLen totalSteps

initialS :: Matrix Char -> S
initialS m =
  S
    { seen = mempty
    , currCoords = coordsForVal 'S' m
    , distance = 0
    }

numReachableCells :: Int -> Int -> Int -> Int
numReachableCells gridLen totalSteps distance =
  let maxGridAmt = (totalSteps - distance) `div` gridLen
      targetParity = totalSteps `mod` 2 -- odd
      distParity = distance `mod` 2 -- even
      gridParity = (targetParity - distParity) `mod` 2 -- odd
      gridAmts = [gridParity, gridParity + 2 .. maxGridAmt]
   in sum . map numPairs $ gridAmts

-- Compute the # of pairs (a, b) such that |a| + |b| = n
numPairs :: Int -> Int
numPairs n
  | n < 0 = 0
  | n == 0 = 1
  | otherwise = zeroPairs + otherPairs
 where
  -- # of pairs with a 0
  -- there are 4 such pairs: (0, n), (0, -n), (n, 0), (-n, 0)
  zeroPairs = 4
  -- # of pairs with NO zeros
  -- See below for explanation
  otherPairs =
    if odd n
      then 8 * half
      else (8 * (half - 1)) + 4
  half = n `div` 2

{-
otherPairs explanation:

ODD CASE:
  We have n//2 pairs which satisfy |a| + |b| = n:
    (1, n-1), (2, n-2), ..., (n//2, n//2 + 1)
  Let A represent this set of n//2 pairs.
  Let (a, b) ∈ A.
  Then, we know that (±a, ±b) AND (±b, ±a) also satisfies |a| + |b| = n.
  (±a, ±b) has 4 possible permutations
  (±b, ±a) has 4 possible permutations
  Thus, the total # of permutations is 8, so the final answer is 8 * (n//2).

EVEN CASE:
  Similar logic as the odd case, EXCEPT we have
    A = {(1, n-1), (2, n-2),..., (n/2 - 1, n/2 + 1)}
  NOTE that (n/2, n/2) ∉ A. We will handle this case separately.
  With the logic in the odd case, A contributes 8 * (n/2 - 1) possible permutations.

  The pair (n/2, n/2) contributes 4 possible permutations, i.e. (±n/2, ±n/2)
-}
