module Day10.Parse (parseMatrix) where

import Control.Monad.Except
import Data.HashMap.Lazy qualified as HashMap

import Day10.Matrix

type Row a = [a]

parseMatrix :: (MonadError String m) => String -> m (Matrix Int)
parseMatrix inputStr = do
  let rows = lines inputStr
  rows' <- mapM (mapM parseDigit) rows
  pure $ rowsToMatrix rows'

parseDigit :: (MonadError String m) => Char -> m Int
parseDigit '0' = pure 0
parseDigit '1' = pure 1
parseDigit '2' = pure 2
parseDigit '3' = pure 3
parseDigit '4' = pure 4
parseDigit '5' = pure 5
parseDigit '6' = pure 6
parseDigit '7' = pure 7
parseDigit '8' = pure 8
parseDigit '9' = pure 9
parseDigit char = throwError $ "Could not parse digit char; invalid char '" ++ show char ++ "'"

rowsToMatrix :: [Row a] -> Matrix a
rowsToMatrix =
  Matrix
    . HashMap.fromList
    . concat
    . mapWithIdx processRow

processRow :: I -> Row a -> [(Coord, a)]
processRow i =
  mapWithIdx (\j a -> ((i, j), a))

mapWithIdx :: (Int -> a -> b) -> [a] -> [b]
mapWithIdx f = zipWith f [0 ..]
