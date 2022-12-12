module Day11.Part01 where

import Control.Monad
import Data.Bool
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List
import Data.List.Split
import Data.Maybe
import GHC.Utils.Misc
import Safe

type Line = String

type Item = Int
type MonkeyIndex = Int

data Monkey = Monkey {
  items :: [Item],
  getNextItem :: Item -> Item,
  getNextKey :: Item -> MonkeyIndex,
  inspectCount :: Int
}

instance Show Monkey where
  show :: Monkey -> String
  show (Monkey {items, inspectCount}) =
    "MONKEY! "
    ++ "inspectCount: " ++ show inspectCount
    ++ " items: " ++ show items

type MonkeyGroup = [Monkey]


filePath :: String
filePath = "data/Day11.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath
  let monkeys = parseMonkeys . splitOn [""] . lines $ inputStr

  let part1Answer = part1 monkeys
  putStrLn $ "PART 1: " ++ show part1Answer

parseMonkeys :: [[Line]] -> IntMap Monkey 
parseMonkeys = IntMap.fromList . mapMaybe parseMonkey

parseMonkey :: [Line] -> Maybe (IntMap.Key, Monkey)
parseMonkey ls =
  let key = parseKey ls
      items = parseItems ls
      operation = parseOperation ls
      test = parseTest ls
      trueMonkey = parseTrueMonkey ls
      falseMonkey = parseFalseMonkey ls
      getNextItem = (.) (`div` 3) <$> operation
      testNextItem = (.) <$> test <*> getNextItem
      getNextKey =
        (.)
          <$> (bool <$> falseMonkey <*> trueMonkey)
          <*> testNextItem
      monkey = Monkey <$> items <*> getNextItem <*> getNextKey <*> pure 0
   in (,) <$> key <*> monkey

parseKey :: [Line] -> Maybe IntMap.Key
parseKey = readMay
         <=< lastMay
         <=< fmap words
         . fmap (filter (/= ':'))
         . headMay

parseItems :: [Line] -> Maybe [Item]
parseItems = fmap (mapMaybe readMay)
           . fmap (splitOn ", ")
           . stripPrefix "  Starting items: "
           <=< flip atMay 1

parseOperation :: [Line] -> Maybe (Int -> Int)
parseOperation = parseOperation' <=< flip atMay 2

parseOperation' :: Line -> Maybe (Int -> Int)
parseOperation' s =
  case (words s) of
       ["Operation:", "new", "=", "old", "+", numStr] -> (+) <$> readMay numStr
       ["Operation:", "new", "=", "old", "*", "old"] -> Just (^ (2 :: Int))
       ["Operation:", "new", "=", "old", "*", numStr] -> (*) <$> readMay numStr
       _ -> Nothing

parseTest :: [Line] -> Maybe (Int -> Bool)
parseTest = parseTest' <=< flip atMay 3

parseTest' :: Line -> Maybe (Int -> Bool)
parseTest' s =
  case (words s) of
       ["Test:", "divisible", "by", numStr] -> divides <$> readMay numStr
       _ -> Nothing

divides :: Int -> Int -> Bool
divides q n = n `mod` q == 0

parseTrueMonkey :: [Line] -> Maybe IntMap.Key
parseTrueMonkey = parseTrueMonkey' <=< flip atMay 4

parseTrueMonkey' :: Line -> Maybe IntMap.Key
parseTrueMonkey' s =
  case (words s) of
       ["If", "true:", "throw", "to", "monkey", numStr] -> readMay numStr
       _ -> Nothing

parseFalseMonkey :: [Line] -> Maybe IntMap.Key
parseFalseMonkey = parseFalseMonkey' <=< flip atMay 5

parseFalseMonkey' :: Line -> Maybe IntMap.Key
parseFalseMonkey' s =
  case (words s) of
       ["If", "false:", "throw", "to", "monkey", numStr] -> readMay numStr
       _ -> Nothing

-- Parts 1 + 2
part1 :: IntMap Monkey -> Int
part1 = monkeyBusiness . doRounds 20

monkeyBusiness :: IntMap Monkey -> Int
monkeyBusiness =
  product
    . take 2
    . reverse
    . sort
    . map inspectCount
    . IntMap.elems

-- Do multiple rounds
doRounds :: Int -> IntMap Monkey -> IntMap Monkey
doRounds n startMonkeys = nTimes n doRound startMonkeys

-- Do a single round
doRound :: IntMap Monkey -> IntMap Monkey
doRound startMonkeys =
  foldl'
    doMonkeyAction 
    startMonkeys
    (IntMap.keys startMonkeys)

-- Action for a single monkey
doMonkeyAction :: IntMap Monkey -> IntMap.Key -> IntMap Monkey
doMonkeyAction monkeys key =
  let monkey = IntMap.lookup key monkeys
      items' = items <$> monkey
      monkeys' = foldr (throwItem key) monkeys <$> items'
   in fromMaybe monkeys monkeys'

throwItem :: IntMap.Key -> Item -> IntMap Monkey -> IntMap Monkey
throwItem key item monkeys =
  let monkey = IntMap.lookup key monkeys
      nextKey = getNextKey <$> monkey <*> pure item
      nextItem = getNextItem <$> monkey <*> pure item
      monkeys' = addItem <$> nextKey <*> nextItem <*> pure monkeys
      monkeys'' = deleteItem key item <$> monkeys'
      monkeys''' = incrementCount key <$> monkeys''
   in fromMaybe monkeys monkeys'''

addItem :: IntMap.Key -> Item -> IntMap Monkey -> IntMap Monkey
addItem key item monkeys =
  IntMap.update
    (\m -> Just $ m { items = item : items m })
    key
    monkeys

deleteItem :: IntMap.Key -> Item -> IntMap Monkey -> IntMap Monkey
deleteItem key item monkeys =
  IntMap.update
    (\m -> Just $ m { items = delete item . items $ m })
    key
    monkeys

incrementCount :: IntMap.Key -> IntMap Monkey -> IntMap Monkey
incrementCount key monkeys =
  IntMap.update
    (\m -> Just $ m { inspectCount = 1 + inspectCount m })
    key
    monkeys
