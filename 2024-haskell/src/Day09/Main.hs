module Day09.Main where

import Control.Monad.Except
import Control.Monad.State.Lazy

import Day09.Parse
import Day09.Part1
import Day09.Part2

filePath :: String
filePath = "src/Day09/data.txt"

main :: IO ()
main =
  runExceptT main' >>= \case
    Left err -> error err
    _ -> pure ()

main' :: (MonadIO m, MonadError String m) => m ()
main' = do
  blocks <- parseBlocks =<< liftIO (readFile filePath)
  let part1Answer = part1 blocks
  liftIO $ putStrLn ("PART 1: " ++ show part1Answer)
  let part2Answer = part2 blocks
  liftIO $ putStrLn ("PART 2: " ++ show part2Answer)
