module Day21.Part01 where

import Data.Char
import Data.Either
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

----------------------
-- SECTION: Main IO --
----------------------
filePath :: String
filePath = "data/Day21.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath
  let exprMap = fromRight emptyExprMap $ parseExprMap inputStr

  let part1Answer = evaluate "root" exprMap
  putStrLn $ "PART 1: " ++ show part1Answer

-------------------------
-- SECTION: Parsing --
-------------------------
type Parser a = Parsec Void String a

type ParserError = ParseErrorBundle String Void

parseExprMap :: String -> Either ParserError ExprMap
parseExprMap s =
  exprMapFromList <$> kvPairs
  where
    kvPairs = runParser kvPairsParser "" s

kvPairsParser :: Parser [(Label, Expr)]
kvPairsParser = kvPairParser `endBy` newline

kvPairParser :: Parser (Label, Expr)
kvPairParser =
  (,)
    <$> (labelParser <* string ": ")
    <*> exprParser

labelParser :: Parser Label
labelParser = takeWhileP Nothing isLetter

exprParser :: Parser Expr
exprParser =
  constParser
    <|> addParser
    <|> subParser
    <|> mulParser
    <|> divParser

constParser :: Parser Expr
constParser = Const <$> L.decimal

addParser :: Parser Expr
addParser =
  Add
    <$> (try $ labelParser <* string " + ")
    <*> labelParser

subParser :: Parser Expr
subParser =
  Sub
    <$> (try $ labelParser <* string " - ")
    <*> labelParser

mulParser :: Parser Expr
mulParser =
  Mul
    <$> (try $ labelParser <* string " * ")
    <*> labelParser

divParser :: Parser Expr
divParser =
  Div
    <$> (try $ labelParser <* string " / ")
    <*> labelParser

-------------------------
-- SECTION: Core Logic --
-------------------------
type Label = String

data Expr
  = Const Integer
  | Add Label Label
  | Sub Label Label
  | Mul Label Label
  | Div Label Label
  deriving (Show)

type ExprMap = HashMap Label Expr

emptyExprMap :: ExprMap
emptyExprMap = HashMap.empty

exprMapFromList :: [(Label, Expr)] -> ExprMap
exprMapFromList = HashMap.fromList

getExpr :: Label -> ExprMap -> Maybe Expr
getExpr = HashMap.lookup

evaluate :: Label -> ExprMap -> Maybe Integer
evaluate l exprMap =
  case (getExpr l exprMap) of
    (Nothing) -> Nothing
    (Just (Const n)) -> Just n
    (Just (Add l1 l2)) -> (+) <$> evaluate l1 exprMap <*> evaluate l2 exprMap
    (Just (Sub l1 l2)) -> (-) <$> evaluate l1 exprMap <*> evaluate l2 exprMap
    (Just (Mul l1 l2)) -> (*) <$> evaluate l1 exprMap <*> evaluate l2 exprMap
    (Just (Div l1 l2)) -> div <$> evaluate l1 exprMap <*> evaluate l2 exprMap
