module Day02.Main where

import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

filePath :: String
filePath = "src/Day02/data.txt"

main :: IO ()
main =
  runExceptT main' >>= \case
    Left err -> error err
    _ -> pure ()

main' :: (MonadIO m, MonadError String m) => m ()
main' = do
  reports <- parseReports =<< liftIO (readFile filePath)

  let part1Answer = part1 reports
  liftIO $ putStrLn ("PART 1: " ++ show part1Answer)

  let part2Answer = part2 reports
  liftIO $ putStrLn ("PART 1: " ++ show part2Answer)

type Report = [Int]

part1 :: [Report] -> Int
part1 = length . filter isSafe

part2 :: [Report] -> Int
part2 = length . filter isSafe2

isSafe :: Report -> Bool
isSafe nums =
  (allIncreasing || allDecreasing)
    && allGradual
 where
  diffs' = diffs nums
  allIncreasing = all (> 0) diffs'
  allDecreasing = all (< 0) diffs'
  allGradual = all isGradual diffs'

diffs :: (Num a) => [a] -> [a]
diffs (x : y : rest) = (y - x) : diffs (y : rest)
diffs _ = []

isGradual :: (Num a, Ord a) => a -> Bool
isGradual n =
  1 <= n' && n' <= 3
 where
  n' = abs n

isSafe2 :: Report -> Bool
isSafe2 nums = isSafe nums || any isSafe otherReports
 where
  otherReports :: [Report]
  otherReports =
    map
      ( \i ->
          let (first, second) = splitAt i nums
           in first ++ drop 1 second
      )
      [0 .. length nums - 1]

-------------------
-- PARSING LOGIC --
-------------------
type MonadParser = MonadParsec Void String

parseReports :: (MonadError String m) => String -> m [Report]
parseReports =
  modifyError show
    . liftEither
    . runParser reportsP ""

reportsP :: (MonadParser m) => m [Report]
reportsP = sepEndBy reportP newline

reportP :: (MonadParser m) => m Report
reportP = sepBy1 L.decimal hspace1
