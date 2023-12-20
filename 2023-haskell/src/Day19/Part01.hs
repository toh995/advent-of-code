module Day19.Part01 where

import Data.Either
import Data.Foldable
import Data.Functor
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.Void
import Text.Megaparsec hiding (label)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Part = Part
  { x, m, a, s :: Int
  }
  deriving (Show)

data Workflow = Workflow
  { label :: Label
  , rules :: [Rule]
  }
  deriving (Show)

emptyWf :: Workflow
emptyWf =
  Workflow
    { label = ""
    , rules = []
    }

newtype Workflows = Workflows
  { hm :: HashMap Label Workflow
  }
  deriving (Show)
instance Semigroup Workflows where
  wf1 <> wf2 = Workflows $ hm wf1 <> hm wf2
instance Monoid Workflows where
  mempty = Workflows mempty

type Label = String

data Rule = Rule
  { test :: Part -> Bool
  , result :: Result
  }
instance Show Rule where
  show = show . result

data Result
  = Accept
  | Reject
  | GoTo Label
  deriving (Eq, Show)

emptyWorkflows :: Workflows
emptyWorkflows = Workflows{hm = mempty}

lookupW :: Workflows -> Label -> Maybe Workflow
lookupW Workflows{hm} =
  flip HashMap.lookup hm

-------------
-- Main IO --
-------------
filePath :: String
filePath = "src/Day19/data.txt"

main :: IO ()
main = do
  (wfs, parts) <- readFile filePath <&> parseAll

  let part1Answer = part1 wfs parts
  putStrLn $ "PART 1: " ++ show part1Answer

----------------
-- Core Logic --
----------------
part1 :: Workflows -> [Part] -> Int
part1 wfs =
  sum
    . map (\Part{x, m, a, s} -> sum [x, m, a, s])
    . filter
      ( (== Just Accept)
          . runWorkflows wfs
      )

runWorkflows :: Workflows -> Part -> Maybe Result
runWorkflows wfs part = do
  startWf <- lookupW wfs "in"
  _runWorkflow wfs part startWf

_runWorkflow :: Workflows -> Part -> Workflow -> Maybe Result
_runWorkflow wfs part wf = do
  rule <- find (`test` part) $ rules wf
  case result rule of
    Accept -> pure Accept
    Reject -> pure Reject
    GoTo label' -> do
      nextWf <- lookupW wfs label'
      _runWorkflow wfs part nextWf

-------------------
-- Parsing Logic --
-------------------
type Parser = Parsec Void String

parseAll :: String -> (Workflows, [Part])
parseAll =
  fromRight (mempty, [])
    . runParser p ""
 where
  p =
    (,)
      <$> (workflowsP <* skipMany newline)
      <*> partsP

workflowsP :: Parser Workflows
workflowsP = do
  wfs <- workflowP `sepBy` singleNewline
  pure
    ( Workflows
        . HashMap.fromList
        . map (\wf -> (label wf, wf))
        $ wfs
    )

workflowP :: Parser Workflow
workflowP = do
  label <- many letterChar <* char '{'
  rules <- rulesP <* char '}'
  pure Workflow{label, rules}

rulesP :: Parser [Rule]
rulesP = ruleP `sepBy` char ','

ruleP :: Parser Rule
ruleP = try rule1P <|> rule2P

rule1P :: Parser Rule
rule1P = do
  ch <- oneOf ['x', 'm', 'a', 's']
  operator <- oneOf ['<', '>']
  num <- L.decimal <* char ':'
  resultStr <- many letterChar
  let test =
        case (ch, operator) of
          ('x', '<') -> \p -> x p < num
          ('x', '>') -> \p -> x p > num
          ('m', '<') -> \p -> m p < num
          ('m', '>') -> \p -> m p > num
          ('a', '<') -> \p -> a p < num
          ('a', '>') -> \p -> a p > num
          ('s', '<') -> \p -> s p < num
          ('s', '>') -> \p -> s p > num
          _ -> const False
  let result =
        case resultStr of
          "A" -> Accept
          "R" -> Reject
          _ -> GoTo resultStr
  pure Rule{test, result}

rule2P :: Parser Rule
rule2P = do
  resultStr <- many letterChar
  let result =
        case resultStr of
          "A" -> Accept
          "R" -> Reject
          _ -> GoTo resultStr
  let test = const True
  pure Rule{result, test}

partsP :: Parser [Part]
partsP = partP `sepEndBy` newline

partP :: Parser Part
partP = do
  _ <- char '{'
  x <- string "x=" *> L.decimal <* char ','
  m <- string "m=" *> L.decimal <* char ','
  a <- string "a=" *> L.decimal <* char ','
  s <- string "s=" *> L.decimal <* char '}'
  pure Part{x, m, a, s}

doubleNewline :: Parser [Char]
doubleNewline = string "\n\n"

singleNewline :: Parser Char
singleNewline = notFollowedBy doubleNewline *> char '\n'
