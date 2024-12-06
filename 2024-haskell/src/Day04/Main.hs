module Day04.Main where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.List
import Text.Regex.TDFA

import Day04.Matrix (Coord, Matrix)
import Day04.Matrix qualified as Matrix

filePath :: String
filePath = "src/Day04/data.txt"

main :: IO ()
main =
  runExceptT main' >>= \case
    Left err -> error err
    _ -> pure ()

main' :: (MonadIO m, MonadError String m) => m ()
main' = do
  fileContents <- liftIO (readFile filePath)
  let part1Answer = part1 fileContents
  liftIO $ putStrLn ("PART 1: " ++ show part1Answer)
  part2Answer <- part2 fileContents
  liftIO $ putStrLn ("PART 2: " ++ show part2Answer)

part1 :: String -> Int
part1 input =
  let rows = lines input
      cols = transpose rows
      diags = Matrix.diags . Matrix.fromRows $ rows
      searchSpaces =
        concat
          [ rows
          , cols
          , diags
          , reverse <$> rows
          , reverse <$> cols
          , reverse <$> diags
          ]
   in sum . map countMatches $ searchSpaces

countMatches :: String -> Int
countMatches s =
  length
    (getAllMatches (s =~ "XMAS") :: [(Int, Int)])

part2 :: (MonadError String m) => String -> m Int
part2 input = do
  let m = Matrix.fromRows $ lines input
  let startCoords =
        [ (i, j)
        | i <- [0 .. m.iMax - 2]
        , j <- [0 .. m.jMax - 2]
        ]
  length <$> filterM (hasXmasCross m) startCoords

hasXmasCross :: (MonadError String m) => Matrix Char -> Coord -> m Bool
hasXmasCross m (i, j) = do
  topL <- Matrix.lookup (i, j) m
  topR <- Matrix.lookup (i, j + 2) m
  mid <- Matrix.lookup (i + 1, j + 1) m
  botL <- Matrix.lookup (i + 2, j) m
  botR <- Matrix.lookup (i + 2, j + 2) m
  pure $
    mid == 'A'
      && (sort [topL, botR] == ['M', 'S'])
      && (sort [topR, botL] == ['M', 'S'])
