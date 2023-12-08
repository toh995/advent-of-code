-- This code here sucks.
-- I ended up moving to solution to the `Main.hs` file in this folder.
-- But I'm keeping this file around for legacy reasons
module Day08.Part02 where

-- import Control.Monad.Trans.Class
-- import Control.Monad.Trans.State.Lazy
import Data.Either
import Data.Functor
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.Hashable
import Data.List
import Data.Maybe
import Data.Void
import GHC.Arr
import GHC.Generics (Generic)
import Safe
import Text.Megaparsec hiding (Pos, State, count, label)
import Text.Megaparsec.Char

newtype Node = Node {label :: String}
  deriving (Eq, Generic, Show)

instance Hashable Node

type StartNode = Node

-- Tree primitives
newtype Tree = Tree (HashMap Node (Node, Node))
  deriving (Show)

emptyTree :: Tree
emptyTree = Tree HashMap.empty

getNodes :: Tree -> [Node]
getNodes (Tree hm) = HashMap.keys hm

data Direction = L | R
  deriving (Show)

type DirArr = Array Int Direction

data Pos = Pos
  { dirIdx :: Int
  , node :: Node
  }
  deriving (Eq, Generic, Show)

instance Hashable Pos

getZindex :: Tree -> DirArr -> StartNode -> Maybe Int
getZindex tree dirs startNode = do
  let startPos =
        Pos
          { node = startNode
          , dirIdx = 0
          }
  let positions =
        iterate
          (nextPos tree dirs)
          startPos
  (idx, _) <-
    headMay
      . filter (("Z" `isSuffixOf`) . snd)
      . zip [0 ..]
      . map (label . node)
      $ positions
  pure idx

nextPos :: Tree -> DirArr -> Pos -> Pos
nextPos
  (Tree treeHm)
  dirs
  Pos{dirIdx, node} =
    Pos
      { dirIdx = dirIdx'
      , node = node'
      }
   where
    dirIdx' = (dirIdx + 1) `mod` length dirs
    node' =
      case dirs ! dirIdx of
        L -> fst $ treeHm HashMap.! node
        R -> snd $ treeHm HashMap.! node

-- IO
filePath :: String
filePath = "src/Day08/data.txt"

main :: IO ()
main = do
  (dirs, tree) <- readAll
  let startNodes =
        filter (("A" `isSuffixOf`) . label)
          . getNodes
          $ tree
  let zIndexes = mapMaybe (getZindex tree dirs) startNodes
  let answer = foldr lcm 1 zIndexes
  putStrLn $ "Part 2: " ++ show answer

readAll :: IO (DirArr, Tree)
readAll =
  readFile filePath
    <&> fromRight (listArray (0, 0) [], emptyTree)
      . runParser allP ""

-- Parsing logic
type Parser = Parsec Void String

allP :: Parser (DirArr, Tree)
allP =
  (,)
    <$> (directionsP <* skipCount 2 newline)
    <*> treeP

directionsP :: Parser DirArr
directionsP = do
  dirs <- many directionP
  pure $ listArray (0, length dirs - 1) dirs

directionP :: Parser Direction
directionP =
  (char 'L' $> L)
    <|> (char 'R' $> R)

treeP :: Parser Tree
treeP =
  lineP
    `sepEndBy` newline
    <&> Tree
      . HashMap.fromList
 where
  lineP = do
    key <- nodeP <* string " = ("
    left <- nodeP <* string ", "
    right <- nodeP <* string ")"
    pure (key, (left, right))

nodeP :: Parser Node
nodeP = Node <$> many (upperChar <|> digitChar)

-- data Cycle = Cycle
--   { cycleLen, offset :: Int
--   }
--   deriving (Eq, Show)
--
-- findCycle :: (Hashable a) => [a] -> Maybe Cycle
-- findCycle as =
--   evalStateT
--     (findCycleS as >> buildCycleS)
--     initialS
--
-- data S a = S
--   { seen :: HashMap a Int
--   , idx :: Int
--   , dupeItemM :: Maybe a
--   }
--
-- initialS :: S a
-- initialS =
--   S
--     { seen = HashMap.empty
--     , idx = 0
--     , dupeItemM = Nothing
--     }
--
-- findCycleS :: (Hashable a) => [a] -> StateT (S a) Maybe ()
-- findCycleS [] = pure ()
-- findCycleS (a : as) = do
--   s@S{seen, idx} <- get
--   if HashMap.member a seen
--     then put s{dupeItemM = Just a}
--     else
--       put
--         s
--           { seen = HashMap.insert a idx seen
--           , idx = idx + 1
--           }
--         >> findCycleS as
--
-- buildCycleS :: (Hashable a) => StateT (S a) Maybe Cycle
-- buildCycleS = do
--   S{seen, idx, dupeItemM} <- get
--   oldIdx <-
--     lift $
--       dupeItemM
--         >>= flip HashMap.lookup seen
--   pure $
--     Cycle
--       { cycleLen = idx - oldIdx
--       , offset = oldIdx
--       }
