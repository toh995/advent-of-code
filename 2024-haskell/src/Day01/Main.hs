module Day01.Main (main) where

import Control.Monad.Except
import Control.Monad.IO.Class

import Day01.Parse
import Day01.Part01
import Day01.Part02

filePath :: String
filePath = "src/Day01/data.txt"

main :: IO ()
main =
  runExceptT main' >>= \case
    Left err -> error err
    _ -> pure ()

main' :: (MonadIO m, MonadError String m) => m ()
main' = do
  rows <- liftIO (readFile filePath) >>= parseRows
  let (column1, column2) = unzip rows

  let part1Answer = part1 column1 column2
  liftIO $ putStrLn ("PART 1: " ++ show part1Answer)

  let part2Answer = part2 column1 column2
  liftIO $ putStrLn ("PART 2: " ++ show part2Answer)
