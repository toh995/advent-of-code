module Day10.Main where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet

import Day10.Matrix (Coord, Matrix)
import Day10.Matrix qualified as Matrix
import Day10.Parse

filePath :: String
filePath = "src/Day10/data.txt"

main :: IO ()
main =
  runExceptT main' >>= \case
    Left err -> error err
    _ -> pure ()

main' :: (MonadIO m, MonadError String m) => m ()
main' = do
  matrix <- parseMatrix =<< liftIO (readFile filePath)
  let part1Answer = part1 matrix
  liftIO $ putStrLn ("PART 1: " ++ show part1Answer)
  let part2Answer = part2 matrix
  liftIO $ putStrLn ("PART 2: " ++ show part2Answer)

minHeight :: Int
minHeight = 0

maxHeight :: Int
maxHeight = 9

part1 :: Matrix Int -> Int
part1 matrix =
  sum
    . map (length . findApexCoords matrix)
    $ startCoords matrix

part2 :: Matrix Int -> Int
part2 matrix =
  sum
    . map (countPaths matrix)
    $ startCoords matrix

findApexCoords :: Matrix Int -> Coord -> HashSet Coord
findApexCoords matrix coord =
  case coord `Matrix.lookup` matrix of
    Nothing -> HashSet.empty
    Just height ->
      if height == maxHeight
        then HashSet.singleton coord
        else
          HashSet.unions
            . map (findApexCoords matrix)
            $ nextCoords matrix coord

countPaths :: Matrix Int -> Coord -> Int
countPaths matrix coord =
  case coord `Matrix.lookup` matrix of
    Nothing -> 0
    Just height ->
      if height == maxHeight
        then 1
        else
          sum
            . map (countPaths matrix)
            $ nextCoords matrix coord

startCoords :: Matrix Int -> [Coord]
startCoords =
  Matrix.coords
    . Matrix.filter (== minHeight)

nextCoords :: Matrix Int -> Coord -> [Coord]
nextCoords matrix coord =
  case coord `Matrix.lookup` matrix of
    Nothing -> []
    Just height ->
      filter (\coord' -> coord' `Matrix.lookup` matrix == Just (height + 1)) $
        Matrix.neighborCoords coord
