module Day04.Matrix where

import Control.Monad.Except
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap

type I = Int
type J = Int
type Coord = (I, J)

data Matrix a = Matrix
  { hm :: HashMap Coord a
  , iMax :: Int
  , jMax :: Int
  }
  deriving (Eq, Show)

empty :: Matrix a
empty =
  Matrix
    { hm = HashMap.empty
    , iMax = -1
    , jMax = -1
    }

fromRows :: [[a]] -> Matrix a
fromRows [] = empty
fromRows rows@(firstRow : _) =
  Matrix
    { iMax = length rows - 1
    , jMax = length firstRow - 1
    , hm = rowsToHashMap rows
    }

rowsToHashMap :: [[a]] -> HashMap Coord a
rowsToHashMap = HashMap.fromList . kvPairs

kvPairs :: [[a]] -> [(Coord, a)]
kvPairs =
  concat
    . zipWith expandRow [0 ..]

expandRow :: I -> [a] -> [((I, J), a)]
expandRow i =
  zipWith
    (\j a -> ((i, j), a))
    [0 ..]

lookup :: (MonadError String m) => Coord -> Matrix a -> m a
lookup coord m =
  case HashMap.lookup coord m.hm of
    Just a -> pure a
    Nothing ->
      throwError $ "Could not find coord " ++ show coord ++ " in matrix"

diags :: Matrix a -> [[a]]
diags = (++) <$> diagsUp <*> diagsDown

diagsUp :: Matrix a -> [[a]]
diagsUp matrix@Matrix{iMax, jMax} =
  singleDiagUp matrix <$> startCoords
 where
  startCoords =
    [(i, 0) | i <- [0 .. iMax - 1]]
      ++ [(iMax, j) | j <- [0 .. jMax]]

singleDiagUp :: Matrix a -> Coord -> [a]
singleDiagUp matrix (i, j) =
  case Day04.Matrix.lookup (i, j) matrix of
    Right a -> a : singleDiagUp matrix (i - 1, j + 1)
    Left _ -> []

diagsDown :: Matrix a -> [[a]]
diagsDown matrix@Matrix{iMax, jMax} =
  singleDiagDown matrix <$> startCoords
 where
  startCoords =
    [(i, 0) | i <- [0 .. iMax]]
      ++ [(0, j) | j <- [1 .. jMax]]

singleDiagDown :: Matrix a -> Coord -> [a]
singleDiagDown matrix (i, j) =
  case Day04.Matrix.lookup (i, j) matrix of
    Right a -> a : singleDiagDown matrix (i + 1, j + 1)
    Left _ -> []
