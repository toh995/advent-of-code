module Day07.Parse (parseEquations) where

import Control.Monad.Except
import Data.Void
import Text.Megaparsec hiding (empty, parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Day07.Types

type MonadParser = MonadParsec Void String

parseEquations :: (MonadError String m) => String -> m [Equation]
parseEquations =
  modifyError show
    . liftEither
    . runParser equationsP ""

equationsP :: (MonadParser m) => m [Equation]
equationsP = sepEndBy equationP newline

equationP :: (MonadParser m) => m Equation
equationP = do
  answer <- L.decimal
  _ <- string ": "
  operands <- sepBy L.decimal (char ' ')
  pure Equation{answer, operands}
