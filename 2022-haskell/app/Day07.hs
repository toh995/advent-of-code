module Day07 where

import Data.List
import Data.Maybe
import Data.Tree
import Text.Read

type Line = String

data Command
  = CD String
  | LS [Entry]
  deriving (Show)

data EntryType = File | Dir
  deriving (Eq, Show)

type Size = Int

type Name = String

data Entry = Entry
  { entryType :: EntryType,
    name :: Name,
    size :: Size
  }
  deriving (Show)

----------------------
-- SECTION: Main IO --
----------------------
filePath :: String
filePath = "data/Day07.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath
  let ls = lines inputStr
  let commands = linesToCommands ls
  let tree = buildTree commands

  let part1Answer = part1 tree
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 tree
  putStrLn $ "PART 2: " ++ show part2Answer

----------------------
-- SECTION: Parsing --
----------------------
linesToCommands :: [Line] -> [Command]
linesToCommands [] = []
linesToCommands (x : xs) =
  case x of
    ('$' : ' ' : 'c' : 'd' : ' ' : dirPath) ->
      CD dirPath : linesToCommands xs
    ('$' : ' ' : 'l' : "s") ->
      LS entries : linesToCommands rest
      where
        paths = takeWhile (not . ("$" `isPrefixOf`)) xs
        entries = mapMaybe lsPathToEntry paths
        rest = dropWhile (not . ("$" `isPrefixOf`)) xs
    _ -> linesToCommands xs

lsPathToEntry :: String -> Maybe Entry
lsPathToEntry path =
  case (words path) of
    ["dir", dirPath] -> Just (Entry Dir dirPath 0)
    [sizeStr, filename] ->
      Entry File filename <$> readMaybe sizeStr
    _ -> Nothing

--------------------------
-- SECTION: Parts 1 + 2 --
--------------------------
part1 :: Tree Entry -> Int
part1 = sum . filter (<= 100000) . getDirSizes

part2 :: Tree Entry -> Int
part2 t =
  let dirSizes = getDirSizes t
      capacity = 70000000
      currUsedSpace = maximum dirSizes
      currFreeSpace = capacity - currUsedSpace
      desiredFreeSpace = 30000000
      minToDelete = desiredFreeSpace - currFreeSpace
      answer = minimum . filter (>= minToDelete) $ dirSizes
   in answer

-------------------------
-- SECTION: Tree Utils --
-------------------------
emptyTree :: Tree Entry
emptyTree = Node (Entry Dir "/" 0) []

getDirSizes :: Tree Entry -> [Size]
getDirSizes t@(Node (Entry {entryType}) children)
  | entryType == Dir = currSize : childSizes
  | otherwise = childSizes
  where
    currSize = treeSize t
    childSizes = concat . map getDirSizes $ children

treeSize :: Tree Entry -> Size
treeSize = sum . fmap size

buildTree :: [Command] -> Tree Entry
buildTree commands = toTree . visitRoot $ finalZipper
  where
    firstZipper = (emptyTree, [])
    finalZipper = foldl' readCommand firstZipper commands

readCommand :: Zipper Entry -> Command -> Zipper Entry
-- CD Commands
readCommand z (CD "/") = visitRoot z
readCommand z (CD "..") = visitParent z
readCommand z (CD dirPath) = upsertChildByName dirPath (Entry Dir dirPath 90) z
-- LS Commands
readCommand z (LS entries) =
  foldl'
    (\z' e@(Entry {name}) -> visitParent $ upsertChildByName name e z')
    z
    entries

-----------------------------
-- SECTION: Tree Traversal --
-----------------------------

-- Adapted from https://blog.josephmorag.com/posts/zip-tree1/
data Crumb a
  = Crumb
      a -- parent node's value
      [Tree a] -- left siblings
      [Tree a] -- right siblings
  deriving (Show)

type Zipper a = (Tree a, [Crumb a])

toTree :: Zipper a -> Tree a
toTree (t, _) = t

visitParent :: Zipper a -> Zipper a
visitParent (focus, []) = (focus, [])
visitParent (focus, Crumb parent left right : cs) =
  (Node parent (left ++ [focus] ++ right), cs)

visitRoot :: Zipper a -> Zipper a
visitRoot (focus, []) = (focus, [])
visitRoot z = visitRoot . visitParent $ z

visitFirstChild :: Zipper a -> Maybe (Zipper a)
visitFirstChild (Node _ [], _) = Nothing
visitFirstChild (Node x (focus : children), crumbs) =
  Just (focus, Crumb x [] children : crumbs)

visitRightSibling :: Zipper a -> Maybe (Zipper a)
visitRightSibling (_, []) = Nothing
visitRightSibling (_, Crumb _ _ [] : _) = Nothing
visitRightSibling (focus, Crumb parent left (r : rs) : cs) =
  Just (r, Crumb parent (left ++ [focus]) rs : cs)

-- Insert child, putting the focus on the new child
insertChild :: a -> Zipper a -> Zipper a
insertChild x (Node parent children, crumbs) =
  (newFocus, newCrumb : crumbs)
  where
    newFocus = Node x []
    newCrumb = Crumb parent [] children

-------------------------------------------
-- SECTION: Special Entry Tree Functions --
-------------------------------------------
upsertChildByName :: Name -> Entry -> Zipper Entry -> Zipper Entry
upsertChildByName name entry z =
  case (visitChildByName name z) of
    (Just z') -> z'
    Nothing -> insertChild entry z

visitChildByName :: Name -> Zipper Entry -> Maybe (Zipper Entry)
visitChildByName n z =
  f firstChild
  where
    firstChild = visitFirstChild z
    f Nothing = Nothing
    f (Just (focus, crumbs))
      | n == (name . rootLabel) focus = Just (focus, crumbs)
      | otherwise = f . visitRightSibling $ (focus, crumbs)
