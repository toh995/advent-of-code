module Day01.Parse (parseRows) where

import Control.Monad.Error.Class
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Day01.Types

type MonadParser = MonadParsec Void String

parseRows :: (MonadError String m) => String -> m [Row]
parseRows =
  modifyError show
    . liftEither
    . runParser rowsP ""

rowsP :: (MonadParser m) => m [Row]
rowsP = sepEndBy rowP newline

rowP :: (MonadParser m) => m Row
rowP = do
  a <- L.decimal
  _ <- hspace1
  b <- L.decimal
  pure (a, b)
