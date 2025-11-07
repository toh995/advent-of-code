{-# LANGUAGE FlexibleContexts #-}

module Day14.Main where

import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Day14.Part1 (part1)
import Day14.Part2 (part2)

filePath :: String
filePath = "src/Day14/data.txt"

main :: IO ()
main = do
    result <- runExceptT main'
    case result of
        Left errMsg -> error errMsg
        Right _ -> pure ()

main' :: (MonadIO m, MonadError String m) => m ()
main' = do
    inputStr <- liftIO $ readFile filePath
    part1Answer <- liftEither $ part1 inputStr
    liftIO $ putStrLn ("PART 1: " ++ show part1Answer)
    part2Answer <- liftEither $ part2 inputStr
    liftIO $ putStrLn ("PART 2: " ++ show part2Answer)
