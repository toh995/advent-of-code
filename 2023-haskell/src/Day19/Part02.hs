module Day19.Part02 where

import Control.Monad
import Data.Either
import Data.Functor
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.Maybe
import Data.Void
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type NodeLabel = String

newtype Graph = Graph
  { hm :: HashMap NodeLabel [Edge]
  }
  deriving (Show)

emptyGraph :: Graph
emptyGraph = Graph{hm = mempty}

data Edge = Edge
  { destLabel :: NodeLabel
  , comps :: [Comp]
  }
  deriving (Eq, Show)

data Position = Position
  { currLabel :: NodeLabel
  , mr :: MultiRange
  }

data Token = X | M | A | S
  deriving (Eq, Show)

data Comp = Comp Token Ordering Int
  deriving (Eq, Show)

data Range = Range {start, end :: Int}
data MultiRange = MultiRange
  { x, m, a, s :: Range
  }

--------
-- IO --
--------
filePath :: String
filePath = "src/Day19/data.txt"

main :: IO ()
main = do
  graph <- parseGraph <$> readFile filePath

  let part2Answer = part2 graph
  putStrLn $ "PART 2: " ++ show part2Answer

part2 :: Graph -> Maybe Int
part2 g =
  countCombos
    . map mr
    <$> findAll g startPos

startPos :: Position
startPos =
  Position
    { currLabel = "in"
    , mr =
        MultiRange
          { x = Range{start = 1, end = 4_000}
          , m = Range{start = 1, end = 4_000}
          , a = Range{start = 1, end = 4_000}
          , s = Range{start = 1, end = 4_000}
          }
    }

countCombos :: [MultiRange] -> Int
countCombos =
  (-)
    <$> (sum . map sizeM)
    <*> countDupes

countDupes :: [MultiRange] -> Int
countDupes [] = 0
countDupes (mr : mrs) =
  let currCount =
        sum
          . map sizeM
          $ mapMaybe (intersectM mr) mrs
   in currCount + countDupes mrs

findAll :: Graph -> Position -> Maybe [Position]
findAll g p
  | currLabel p == "A" = Just [p]
  | currLabel p == "R" = Just []
  | otherwise = do
      edges <- lookupG g . currLabel $ p
      let nextPs = mapMaybe (move p) edges
      pure
        ( concat
            . mapMaybe (findAll g)
            $ nextPs
        )

move :: Position -> Edge -> Maybe Position
move p edge = do
  mr' <- foldM applyMulti (mr p) (comps edge)
  pure $
    p
      { mr = mr'
      , currLabel = destLabel edge
      }

applyMulti :: MultiRange -> Comp -> Maybe MultiRange
applyMulti mr comp@(Comp X _ _) = applyRange (x mr) comp <&> \r -> mr{x = r}
applyMulti mr comp@(Comp M _ _) = applyRange (m mr) comp <&> \r -> mr{m = r}
applyMulti mr comp@(Comp A _ _) = applyRange (a mr) comp <&> \r -> mr{a = r}
applyMulti mr comp@(Comp S _ _) = applyRange (s mr) comp <&> \r -> mr{s = r}

applyRange :: Range -> Comp -> Maybe Range
applyRange r (Comp _ LT n)
  | start r >= n = Nothing
  | otherwise = Just r{end = min (end r) (n - 1)}
applyRange r (Comp _ GT n)
  | end r <= n = Nothing
  | otherwise = Just r{start = max (start r) (n + 1)}
applyRange r (Comp _ EQ n)
  | n < start r || n > end r = Nothing
  | otherwise = Just r{start = n, end = n}

sizeM :: MultiRange -> Int
sizeM MultiRange{x, m, a, s} =
  product
    . map sizeR
    $ [x, m, a, s]

sizeR :: Range -> Int
sizeR Range{start, end} = end - start + 1

intersectM :: MultiRange -> MultiRange -> Maybe MultiRange
intersectM mr1 mr2 = do
  x <- intersectR (x mr1) (x mr2)
  m <- intersectR (m mr1) (m mr2)
  a <- intersectR (a mr1) (a mr2)
  s <- intersectR (s mr1) (s mr2)
  pure $ MultiRange{x, m, a, s}

intersectR :: Range -> Range -> Maybe Range
intersectR
  (Range{start = s1, end = e1})
  (Range{start = s2, end = e2})
    | s1 < s2 =
        if e1 < s2
          then Nothing
          else Just Range{start = s2, end = min e1 e2}
    | s1 > e2 = Nothing
    | otherwise = Just Range{start = s1, end = min e1 e2}

lookupG :: Graph -> NodeLabel -> Maybe [Edge]
lookupG Graph{hm} =
  flip HashMap.lookup hm

invert :: Comp -> Comp
invert (Comp t LT n) = Comp t GT (n - 1)
invert (Comp t GT n) = Comp t LT (n + 1)
invert (Comp t EQ n) = Comp t EQ n

-------------------
-- Parsing Logic --
-------------------
type Parser = Parsec Void String

parseGraph :: String -> Graph
parseGraph =
  fromRight emptyGraph
    . runParser graphP ""

graphP :: Parser Graph
graphP =
  Graph
    . HashMap.fromList
    <$> graphEntryP
      `sepBy` singleNewline

graphEntryP :: Parser (NodeLabel, [Edge])
graphEntryP =
  (,)
    <$> nodeLabelP
    <*> between
      (char '{')
      (char '}')
      (edgesP [])

edgesP :: [Comp] -> Parser [Edge]
edgesP prevComps =
  try p1 <|> p2
 where
  p1 = do
    comp <- compP <* char ':'
    let comp' = invert comp
    destLabel <- nodeLabelP <* skipMany (char ',')
    let currEdge = Edge{destLabel, comps = comp : prevComps}
    (:) currEdge <$> edgesP (comp' : prevComps)
  p2 =
    nodeLabelP
      <&> \lab ->
        [ Edge
            { destLabel = lab
            , comps = prevComps
            }
        ]

nodeLabelP :: Parser NodeLabel
nodeLabelP = some letterChar

compP :: Parser Comp
compP =
  Comp
    <$> tokenP
    <*> orderingP
    <*> L.decimal

tokenP :: Parser Token
tokenP =
  (char 'x' $> X)
    <|> (char 'm' $> M)
    <|> (char 'a' $> A)
    <|> (char 's' $> S)

orderingP :: Parser Ordering
orderingP =
  (char '<' $> LT)
    <|> (char '>' $> GT)

doubleNewline :: Parser [Char]
doubleNewline = string "\n\n"

singleNewline :: Parser Char
singleNewline = notFollowedBy doubleNewline *> char '\n'
