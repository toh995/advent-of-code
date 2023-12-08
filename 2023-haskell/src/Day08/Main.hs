module Day08.Main where

import Control.Monad.Trans.State.Lazy
import Data.Either
import Data.Functor
import Data.HashMap.Lazy (HashMap, (!))
import Data.HashMap.Lazy qualified as HashMap
import Data.Hashable
import Data.InfList (InfList)
import Data.InfList qualified as InfList
import Data.List
import Data.Void
import Text.Megaparsec hiding (State, count, label)
import Text.Megaparsec.Char

newtype Node = Node {label :: String}
  deriving (Eq, Show)

instance Hashable Node where
  hashWithSalt n = hashWithSalt n . label
  hash = hash . label

newtype Tree = Tree (HashMap Node (Node, Node))
  deriving (Show)

data Direction = L | R
  deriving (Show)

type Count = Int

filePath :: String
filePath = "src/Day08/data.txt"

main :: IO ()
main = do
  (ds, tree) <- readAll

  let part1Answer = part1 tree ds
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 tree ds
  putStrLn $ "PART 2: " ++ show part2Answer

readAll :: IO ([Direction], Tree)
readAll =
  readFile filePath
    <&> fromRight ([], emptyTree)
      . runParser allP ""

-- Tree primitives
emptyTree :: Tree
emptyTree = Tree HashMap.empty

getNodes :: Tree -> [Node]
getNodes (Tree hm) = HashMap.keys hm

getNextNode :: Direction -> Tree -> Node -> Node
getNextNode L (Tree hm) node = fst $ hm ! node
getNextNode R (Tree hm) node = snd $ hm ! node

data S = S
  { tree :: Tree
  , node :: Node
  , ds :: InfList Direction
  , count :: Count
  }

instance Show S where
  show (S{tree, node, count}) = show tree ++ show node ++ show count

part1 :: Tree -> [Direction] -> Count
part1 tree ds =
  count finalS
 where
  (_, finalS) =
    runState allMovesS1 initialS
  initialS =
    S
      { tree
      , node = Node "AAA"
      , ds = InfList.cycle ds
      , count = 0
      }

allMovesS1 :: State S ()
allMovesS1 =
  get >>= \S{node} ->
    if node == Node "ZZZ"
      then pure ()
      else moveS >> allMovesS1

part2 :: Tree -> [Direction] -> Count
part2 tree ds =
  let initialNodes =
        filter (("A" `isSuffixOf`) . label)
          . getNodes
          $ tree
      zIndexes = map (getZindex2 tree ds) initialNodes
   in foldr lcm 1 zIndexes

getZindex2 :: Tree -> [Direction] -> Node -> Count
getZindex2 tree ds node =
  count finalS
 where
  (_, finalS) =
    runState allMovesS2 initialS
  initialS =
    S
      { tree
      , node
      , ds = InfList.cycle ds
      , count = 0
      }

allMovesS2 :: State S ()
allMovesS2 =
  get >>= \S{node} ->
    if ("Z" `isSuffixOf`) . label $ node
      then pure ()
      else moveS >> allMovesS2

moveS :: State S ()
moveS = do
  S{tree, node, ds, count} <- get
  let nextNode =
        getNextNode (InfList.head ds) tree node
  put $
    S
      { tree
      , node = nextNode
      , ds = InfList.tail ds
      , count = count + 1
      }

-- Parsing logic
type Parser = Parsec Void String

allP :: Parser ([Direction], Tree)
allP =
  (,)
    <$> (many directionP <* skipCount 2 newline)
    <*> treeP

treeP :: Parser Tree
treeP =
  lineP `sepEndBy` newline
    <&> Tree . HashMap.fromList
 where
  lineP = do
    key <- nodeP <* string " = ("
    left <- nodeP <* string ", "
    right <- nodeP <* string ")"
    pure (key, (left, right))

nodeP :: Parser Node
nodeP = Node <$> many (upperChar <|> digitChar)

directionP :: Parser Direction
directionP =
  (char 'L' $> L)
    <|> (char 'R' $> R)
