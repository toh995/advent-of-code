module Day10.Matrix where

import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap

type I = Int
type J = Int
type Coord = (I, J)

newtype Matrix a = Matrix (HashMap Coord a)
  deriving (Eq, Show)

empty :: Matrix a
empty = Matrix HashMap.empty

lookup :: Coord -> Matrix a -> Maybe a
lookup coord (Matrix hm) = coord `HashMap.lookup` hm

filter :: (a -> Bool) -> Matrix a -> Matrix a
filter f (Matrix hm) = Matrix $ HashMap.filter f hm

coords :: Matrix a -> [Coord]
coords (Matrix hm) = HashMap.keys hm

neighborCoords :: Coord -> [Coord]
neighborCoords (i, j) =
  [ (i + 1, j)
  , (i - 1, j)
  , (i, j + 1)
  , (i, j - 1)
  ]
