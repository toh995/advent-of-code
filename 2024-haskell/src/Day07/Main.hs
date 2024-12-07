module Day07.Main where

import Control.Monad.Except
import Control.Monad.IO.Class

import Day07.Parse
import Day07.Part1
import Day07.Part2

filePath :: String
filePath = "src/Day07/data.txt"

main :: IO ()
main =
  runExceptT main' >>= \case
    Left err -> error err
    _ -> pure ()

main' :: (MonadIO m, MonadError String m) => m ()
main' = do
  equations <- parseEquations =<< liftIO (readFile filePath)
  let part1Answer = part1 equations
  liftIO $ putStrLn ("PART 1: " ++ show part1Answer)
  let part2Answer = part2 equations
  liftIO $ putStrLn ("PART 2: " ++ show part2Answer)
