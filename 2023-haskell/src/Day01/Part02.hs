module Day01.Part02 where

import Data.Functor
import Data.Maybe
import Data.Void
import GHC.Data.Maybe
import Safe
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Read

import Util.Types

part2 :: [Line] -> Int
part2 = sum . mapMaybe extractInt

extractInt :: Line -> Maybe Int
extractInt l = do
  digitChars <- rightToMaybe . runParser multiDigit "" $ l
  first <- headMay digitChars
  second <- lastMay digitChars
  readMaybe $ [first, second]

-- Use parser combinators to parse out the digit chars
type Parser = Parsec Void String

multiDigit :: Parser [Char]
multiDigit =
  (eof $> [])
    <|> ( do
            d <- lookAhead singleDigit
            _ <- anySingle
            (:) d <$> multiDigit
        )
    <|> (anySingle >> multiDigit)

singleDigit :: Parser Char
singleDigit =
  digitChar
    <|> ('1' <$ string "one")
    <|> ('2' <$ string "two")
    <|> ('3' <$ string "three")
    <|> ('4' <$ string "four")
    <|> ('5' <$ string "five")
    <|> ('6' <$ string "six")
    <|> ('7' <$ string "seven")
    <|> ('8' <$ string "eight")
    <|> ('9' <$ string "nine")

-- Here is an alternative method without using parser combinators

-- extractDigits :: Line -> [Char]
-- extractDigits [] = []
-- extractDigits (c : cs)
--   | isDigit c = c : extractDigits cs
--   | "one" `isPrefixOf` (c : cs) = '1' : extractDigits cs
--   | "two" `isPrefixOf` (c : cs) = '2' : extractDigits cs
--   | "three" `isPrefixOf` (c : cs) = '3' : extractDigits cs
--   | "four" `isPrefixOf` (c : cs) = '4' : extractDigits cs
--   | "five" `isPrefixOf` (c : cs) = '5' : extractDigits cs
--   | "six" `isPrefixOf` (c : cs) = '6' : extractDigits cs
--   | "seven" `isPrefixOf` (c : cs) = '7' : extractDigits cs
--   | "eight" `isPrefixOf` (c : cs) = '8' : extractDigits cs
--   | "nine" `isPrefixOf` (c : cs) = '9' : extractDigits cs
--   | otherwise = extractDigits cs
