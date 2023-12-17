module Day14.Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Either
import Data.Foldable
import Data.Functor
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.Hashable
import Data.List
import Data.Void
import GHC.Generics (Generic)
import Safe
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

-- Matrix primitives
type I = Int
type J = Int
type Coord = (I, J)

data Matrix a = Matrix
  { iMax, jMax :: Int
  , hm :: HashMap Coord a
  }
  deriving (Eq, Generic, Show)
instance (Hashable a, Eq a) => Hashable (Matrix a)

data Direction = U | D | L | R
  deriving (Eq, Ord, Show)

emptyMatrix :: Matrix a
emptyMatrix =
  Matrix
    { iMax = 0
    , jMax = 0
    , hm = mempty
    }

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

insertM :: Matrix a -> Coord -> a -> Matrix a
insertM matrix coord val =
  matrix
    { hm = HashMap.insert coord val (hm matrix)
    }

getCoordsForVal :: (Eq a) => a -> Matrix a -> [Coord]
getCoordsForVal a m =
  filter ((== Just a) . lookupM m)
    . HashMap.keys
    . hm
    $ m

moveCoord :: Direction -> Coord -> Coord
moveCoord U (i, j) = (i - 1, j)
moveCoord D (i, j) = (i + 1, j)
moveCoord L (i, j) = (i, j - 1)
moveCoord R (i, j) = (i, j + 1)

getRowCoords :: Matrix a -> [[Coord]]
getRowCoords Matrix{iMax, jMax} =
  let is = [0 .. iMax]
      js = [0 .. jMax]
   in map
        (\i -> (i,) <$> js)
        is

getColCoords :: Matrix a -> [[Coord]]
getColCoords = transpose . getRowCoords

-- Main IO
filePath :: String
filePath = "src/Day14/data.txt"

main :: IO ()
main = do
  matrix <- readFile filePath <&> parseMatrix

  let part1Answer = part1 matrix
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 matrix
  putStrLn $ "PART 2: " ++ show part2Answer

-- Core Logic
data Tile = Round | Cube | Empty
  deriving (Eq, Generic, Show)

instance Hashable Tile

part1 :: Matrix Tile -> Int
part1 =
  computeLoad . slideAll U

part2 :: Matrix Tile -> Maybe Int
part2 matrix = do
  let matrices = iterate revolve matrix
  (i, i') <- findFirstDupeIdxes matrices
  let cycleLen = i' - i
  let finalIdx = ((1000000000 - i) `mod` cycleLen) + i
  finalMatrix <- matrices `atMay` finalIdx
  pure $ computeLoad finalMatrix

computeLoad :: Matrix Tile -> Int
computeLoad m@(Matrix{iMax}) =
  sum
    . map (\(i, _) -> iMax + 1 - i)
    . getCoordsForVal Round
    $ m

revolve :: Matrix Tile -> Matrix Tile
revolve m =
  foldl'
    (flip slideAll)
    m
    [U, L, D, R]

slideAll :: Direction -> Matrix Tile -> Matrix Tile
slideAll dir matrix =
  foldl'
    (slide dir)
    matrix
    allCoords
 where
  allCoords = case dir of
    U -> concat colCoords
    D -> concatMap reverse colCoords
    L -> concat rowCoords
    R -> concatMap reverse rowCoords
  rowCoords = getRowCoords matrix
  colCoords = getColCoords matrix

slide :: Direction -> Matrix Tile -> Coord -> Matrix Tile
slide dir matrix currCoord
  | shouldSlide = nextMatrix
  | otherwise = matrix
 where
  currTileM = lookupM matrix currCoord
  nextCoord = moveCoord dir currCoord
  nextTileM = lookupM matrix nextCoord
  shouldSlide =
    currTileM == Just Round
      && nextTileM == Just Empty
  matrix' = insertM matrix currCoord Empty
  matrix'' = insertM matrix' nextCoord Round
  nextMatrix = slide dir matrix'' nextCoord

type Idx = Int

findFirstDupeIdxes :: (Eq a, Hashable a) => [a] -> Maybe (Int, Int)
findFirstDupeIdxes xs =
  evalStateT
    (fooS xs 0)
    HashMap.empty
 where
  fooS :: (Hashable a) => [a] -> Idx -> StateT (HashMap a Idx) Maybe (Idx, Idx)
  fooS [] _ = lift Nothing
  fooS (a : as) idx = do
    idxMap <- get
    let prevIdxM = HashMap.lookup a idxMap
    put $ HashMap.insert a idx idxMap
    case prevIdxM of
      Just prevIdx -> pure (prevIdx, idx)
      Nothing -> fooS as (idx + 1)

-- Parsing logic
type Parser = Parsec Void String

parseMatrix :: String -> Matrix Tile
parseMatrix =
  fromRight emptyMatrix
    . runParser matrixP ""

matrixP :: Parser (Matrix Tile)
matrixP =
  buildMatrix
    <$> some tileP `sepEndBy` newline

tileP :: Parser Tile
tileP =
  (char 'O' $> Round)
    <|> (char '#' $> Cube)
    <|> (char '.' $> Empty)
