module Day04.Main where

import Control.Monad.Except
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap

main :: IO ()
main = print "hi"

part1 :: (MonadError String m) => Matrix Char -> m Int
part1 matrix = do
  let
    coordGroups :: [[Coord]]
    coordGroups =
      concatMap
        ($ matrix)
        [rowCoords, colCoords, diagCoordsUp, diagCoordsDown]

  -- searchSpaces :: [String]
  -- searchSpaces =
  pure 0

allSearchSpaces :: (MonadError String m) => Matrix Char -> m [String]
allSearchSpaces matrix = do
  let coordGroups =
        concatMap
          ($ matrix)
          [rowCoords, colCoords, diagCoordsUp, diagCoordsDown]
  mapM (mapM (`Day04.Main.lookup` matrix)) coordGroups

type Coord = (Int, Int)

data Matrix a = Matrix
  { hm :: HashMap Coord a
  , iMax :: Int
  , jMax :: Int
  }

instance (Show a) => Show (Matrix a) where
  show = show . hm

lookup :: (MonadError String m, Show a) => Coord -> Matrix a -> m a
lookup coord m =
  case HashMap.lookup coord m.hm of
    Just a -> pure a
    Nothing -> throwError $ "Could not find coord " ++ show coord ++ " in matrix" ++ show m

rowCoords :: Matrix a -> [[Coord]]
rowCoords Matrix{iMax, jMax} =
  [singleRow i | i <- [0 .. iMax]]
 where
  singleRow i = [(i, j) | j <- [0 .. jMax]]

colCoords :: Matrix a -> [[Coord]]
colCoords Matrix{iMax, jMax} =
  [singleCol j | j <- [0 .. jMax]]
 where
  singleCol j = [(i, j) | i <- [0 .. iMax]]

diagCoordsUp :: Matrix a -> [[Coord]]
diagCoordsUp Matrix{iMax, jMax} =
  diag <$> startCoords
 where
  diag (i0, j0) =
    [ (i0 - delta, j0 + delta)
    | delta <- [0 .. min iMax jMax]
    , (i0 - delta) >= 0
    , (j0 + delta) <= jMax
    ]
  startCoords =
    [(i, 0) | i <- [0 .. iMax]]
      ++ [(iMax, j) | j <- [0 .. jMax]]

diagCoordsDown :: Matrix a -> [[Coord]]
diagCoordsDown Matrix{iMax, jMax} =
  diag <$> startCoords
 where
  diag (i0, j0) =
    [ (i0 + delta, j0 + delta)
    | delta <- [0 .. min iMax jMax]
    , (i0 + delta) >= iMax
    , (j0 + delta) <= jMax
    ]
  startCoords =
    [(i, 0) | i <- [0 .. iMax]]
      ++ [(0, j) | j <- [0 .. jMax]]
