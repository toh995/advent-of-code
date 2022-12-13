module Day13 where

import Control.Applicative
import Data.Char
import Data.List.Split
import Data.Maybe
import Data.List
import GHC.Utils.Misc

----------------------------------------
-- SECTION: Tree Definition and Utils --
----------------------------------------
data Tree a = Leaf a
            | Node [Tree a]
  deriving (Eq, Show)

treeFromVal :: a -> Tree a
treeFromVal = Leaf

treeFromForest :: [Tree a] -> Tree a
treeFromForest = Node

treeFromList :: [a] -> Tree a
treeFromList xs = Node $ map Leaf xs

treeFromNestedList :: [[a]] -> Tree a
treeFromNestedList xss = Node $ map treeFromList xss

instance Ord a => Ord (Tree a) where
  compare :: Tree a -> Tree a -> Ordering
  compare (Leaf x)   (Leaf y)   = compare x y
  compare t@(Leaf _) (Node ts)  = compare [t] ts
  compare (Node ts)  t@(Leaf _) = compare ts [t]
  compare (Node ts)  (Node us)  = compare ts us

----------------------
-- SECTION: Main IO --
----------------------
filePath :: String
filePath = "data/Day13.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath
  let treePairs = getTreePairs inputStr
  let trees = getTrees inputStr

  let part1Answer = part1 treePairs
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 trees
  putStrLn $ "PART 2: " ++ show part2Answer

----------------------------
-- SECTION: Input Parsing --
----------------------------
getTrees :: String -> [Tree Integer]
getTrees =
  mapMaybe strToTree
    . filter (not . null)
    . lines

getTreePairs :: String -> [(Tree Integer, Tree Integer)]
getTreePairs =
  mapMaybe listToPair
    . map (mapMaybe strToTree)
    . splitOn [""] 
    . lines

listToPair :: [a] -> Maybe (a, a)
listToPair xs
  | length xs /= 2 = Nothing
  | otherwise      = Just (xs !! 0, xs !! 1)

--------------------------
-- SECTION: Parts 1 + 2 --
--------------------------
part1 :: Ord a => [(Tree a, Tree a)] -> Integer
part1 =
  sum
    . map fst
    . filter (uncurry (<) . snd)
    . zip [1..]

part2 :: [Tree Integer] -> Integer
part2 ts =
  let t = treeFromNestedList [[2]]
      u = treeFromNestedList [[6]]
      sorted = sort $ t : u : ts
      tIdx = (+ 1) <$> findIndex (== t) sorted
      uIdx = (+ 1) <$> findIndex (== u) sorted
      result = (*) <$> tIdx <*> uIdx
      result' = toInteger <$> result
   in fromMaybe 0 result'

---------------------------
-- SECTION: Tree Parsing --
---------------------------
-- Parse string to tree
-- Inspired by serokell's blog post on parser combinators:
-- https://serokell.io/blog/parser-combinators-in-haskell
data Parser a = Parser {runParser :: String -> Maybe (a, String)}

inParser ::
  ((String -> Maybe (a1, String)) -> String -> Maybe (a2, String))
  -> Parser a1
  -> Parser a2
inParser f = Parser . f . runParser

instance Functor Parser where
  fmap = inParser . fmap . fmap . liftFst

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  (Parser fp) <*> xp = Parser $ \s ->
    case fp s of
      Nothing     -> Nothing
      Just (f,s') -> runParser (f <$> xp) s'

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

parseTree :: Parser (Tree Integer)
parseTree = parseLeaf <|> (treeFromForest <$> parseForest)

parseLeaf :: Parser (Tree Integer)
parseLeaf =
  zeroOrMore (char ',')
    *> (treeFromVal <$> posInt)
    <* zeroOrMore (char ',')

parseForest :: Parser [Tree Integer]
parseForest =
  zeroOrMore (char ',')
    *> char '['
    *> zeroOrMore (parseLeaf <|> parseTree)
    <* char ']'
    <* zeroOrMore (char ',')

strToTree :: String -> Maybe (Tree Integer)
strToTree s = fst <$> runParser parseTree s
