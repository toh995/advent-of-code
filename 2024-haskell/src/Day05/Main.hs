module Day05.Main where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.List
import Data.Void
import Safe
import Text.Megaparsec hiding (empty, parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

filePath :: String
filePath = "src/Day05/data.txt"

main :: IO ()
main =
  runExceptT main' >>= \case
    Left err -> error err
    _ -> pure ()

main' :: (MonadIO m, MonadError String m) => m ()
main' = do
  (rules, pageLists) <- parse =<< liftIO (readFile filePath)
  part1Answer <- part1 rules pageLists
  liftIO $ putStrLn ("PART 1: " ++ show part1Answer)
  part2Answer <- part2 rules pageLists
  liftIO $ putStrLn ("PART 2: " ++ show part2Answer)

part1 :: (MonadError String m) => OrderRules -> [[PageNum]] -> m Int
part1 rules =
  fmap sum
    . mapM middleVal
    . filter (isSorted rules)

part2 :: (MonadError String m) => OrderRules -> [[PageNum]] -> m Int
part2 rules =
  fmap sum
    . mapM (middleVal . pageSort rules)
    . filter (not . isSorted rules)

middleVal :: (MonadError String m) => [a] -> m a
middleVal as
  | odd len =
      case atMay as (len `div` 2) of
        Just res -> pure res
        Nothing -> throwError $ "Invalid index for list " ++ show (len `div` 2)
  | otherwise = throwError "Cannot find middle value of even-length list"
 where
  len = length as

type PageNum = Int

data OrderRule = OrderRule {before, after :: PageNum}
  deriving (Show)

newtype OrderRules = OrderRules
  { hm :: HashMap (PageNum, PageNum) OrderRule
  }
  deriving (Show)

empty :: OrderRules
empty = OrderRules{hm = mempty}

buildOrderRules :: [OrderRule] -> OrderRules
buildOrderRules = foldr insertRule empty

lookupRule :: (PageNum, PageNum) -> OrderRules -> Maybe OrderRule
lookupRule (a1, a2) =
  HashMap.lookup (sortPair (a1, a2))
    . hm

insertRule :: OrderRule -> OrderRules -> OrderRules
insertRule rule rules =
  rules
    { hm =
        HashMap.insert
          (sortPair (rule.before, rule.after))
          rule
          rules.hm
    }

sortPair :: (Ord a) => (a, a) -> (a, a)
sortPair (a1, a2)
  | a1 < a2 = (a1, a2)
  | otherwise = (a2, a1)

pageSort :: OrderRules -> [PageNum] -> [PageNum]
pageSort rules = sortBy sortFn
 where
  sortFn a1 a2 =
    case lookupRule (a1, a2) rules of
      Nothing -> EQ
      Just rule
        | a1 == rule.before -> LT
        | otherwise -> GT

isSorted :: OrderRules -> [PageNum] -> Bool
isSorted rules pageNums =
  pageNums == pageSort rules pageNums

type MonadParser = MonadParsec Void String

parse :: (MonadError String m) => String -> m (OrderRules, [[PageNum]])
parse =
  modifyError show
    . liftEither
    . runParser parser ""
 where
  parser =
    (,)
      <$> (rulesP <* newline)
      <*> pageListsP

rulesP :: (MonadParser m) => m OrderRules
rulesP = buildOrderRules <$> sepEndBy ruleP newline

ruleP :: (MonadParser m) => m OrderRule
ruleP = do
  before <- L.decimal
  _ <- char '|'
  after <- L.decimal
  pure OrderRule{before, after}

pageListsP :: (MonadParser m) => m [[PageNum]]
pageListsP = sepEndBy pageListP newline

pageListP :: (MonadParser m) => m [PageNum]
pageListP = sepBy1 L.decimal (char ',')
