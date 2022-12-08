module Day08 where

import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Utils.Misc

-- Not to be confused with the data structure called "Tree"!
data Tree = Tree
  { i :: Int,
    j :: Int,
    height :: Int
  }
  deriving (Eq, Show)

-- Define lexical ordering on the i and j coordinates
instance Ord Tree where
  compare t1 t2 = compare (i t1, j t1) (i t2, j t2)

-- Define a "Matrix" type, which is a semigroup under the
-- "Hadamard product" operation.
--
-- FOR EXAMPLE:
--  1 2 <> 2 2 == (1*2) (2*2) == 2 4
--  3 4    2 2    (3*2) (4*2)    6 8
data Matrix a = Matrix [[a]]

instance Num a => Semigroup (Matrix a) where
  (<>) (Matrix rows1) (Matrix rows2) =
    Matrix $
      zipWith (zipWith (*)) rows1 rows2

-- Fold the matrix, by iterating through all of the entries in the matrix
instance Foldable Matrix where
  foldr :: (a -> b -> b) -> b -> Matrix a -> b
  foldr f z (Matrix rows) = foldr f' z rows
    where
      f' row acc = foldr f acc row

----------------------
-- SECTION: Main IO --
----------------------
filePath :: String
filePath = "data/Day08.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath
  let treeMatrix = parseTreeMatrix inputStr

  let part1Answer = part1 treeMatrix
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 treeMatrix
  putStrLn $ "PART 2: " ++ show part2Answer

----------------------
-- SECTION: Parsing --
----------------------
parseTreeMatrix :: String -> [[Tree]]
parseTreeMatrix =
  intsToTrees . charsToInts . lines

charsToInts :: [[Char]] -> [[Int]]
charsToInts = map (map digitToInt)

intsToTrees :: [[Int]] -> [[Tree]]
intsToTrees =
  map (uncurry buildRow)
    . zip [0 ..]

buildRow :: Int -> [Int] -> [Tree]
buildRow i heights =
  map (uncurry3 Tree)
    . zip3 (repeat i) [0 ..]
    $ heights

---------------------
-- SECTION: Part 1 --
---------------------
part1 :: [[Tree]] -> Int
part1 rows =
  let columns = transpose rows
      visibleFromRows = mconcat . map visibleTrees $ rows
      visibleFromCols = mconcat . map visibleTrees $ columns
   in Set.size $ visibleFromRows <> visibleFromCols

visibleTrees :: [Tree] -> Set Tree
visibleTrees ts =
  Set.union lSet rSet
  where
    (lSet, _) = foldl' f (Set.empty, minBound) ts
    (rSet, _) = foldr (flip f) (Set.empty, minBound) ts

    f :: (Set Tree, Int) -> Tree -> (Set Tree, Int)
    f (s, maxHeight) t@(Tree {height})
      | height > maxHeight = (Set.insert t s, height)
      | otherwise = (s, maxHeight)

---------------------
-- SECTION: Part 2 --
---------------------
part2 :: [[Tree]] -> Int
part2 rows =
  let columns = transpose rows
      leftMatrix = Matrix $ map leftDistances $ rows
      rightMatrix = Matrix $ map rightDistances $ rows
      upMatrix = Matrix $ transpose . map leftDistances $ columns
      downMatrix = Matrix $ transpose . map rightDistances $ columns
   in maximum $ leftMatrix <> rightMatrix <> upMatrix <> downMatrix

-- For each tree, compute the viewing distance, looking to the right
rightDistances :: [Tree] -> [Int]
rightDistances =
  fst . foldr f ([], [])
  where
    f :: Tree -> ([Int], [Tree]) -> ([Int], [Tree])
    f t (accum, seen) = (currDist : accum, t : seen)
      where
        currDist = getDistance t seen

-- For each tree, compute the viewing distance, looking to the left
leftDistances :: [Tree] -> [Int]
leftDistances =
  fst . foldl' f ([], [])
  where
    f :: ([Int], [Tree]) -> Tree -> ([Int], [Tree])
    f (accum, seen) t = (accum ++ [currDist], t : seen)
      where
        currDist = getDistance t seen

getDistance :: Tree -> [Tree] -> Int
getDistance t ts
  | remaining == [] = length smallerNeighbors
  | otherwise = 1 + length smallerNeighbors
  where
    smallerNeighbors = takeWhile (`hlessThan` t) ts
    remaining = dropWhile (`hlessThan` t) ts

hlessThan :: Tree -> Tree -> Bool
hlessThan t1 t2 = height t1 < height t2
