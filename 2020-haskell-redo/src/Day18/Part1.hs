module Day18.Part1 where

import Control.Category ((>>>))
import Text.Parsec (Parsec, between, choice, many, many1, parse, try)
import qualified Text.Parsec.Char as C

part1 :: String -> Int
part1 =
    lines
        >>> map eval
        >>> sum

eval :: String -> Int
eval s =
    case parse expressionP "" s of
        Right ret -> ret
        Left parseErr -> error $ show parseErr

type Parser = Parsec String ()

expressionP :: Parser Int
expressionP = do
    firstTerm <- termP
    rest <- many $ try ((,) <$> opP <*> termP)
    pure $
        foldl
            (\acc (op, term) -> op acc term)
            firstTerm
            rest

opP :: Parser (Int -> Int -> Int)
opP =
    choice
        [ (+) <$ (try $ C.string " + ")
        , (*) <$ (try $ C.string " * ")
        ]

termP :: Parser Int
termP =
    choice
        [ try intP
        , try $ between (C.char '(') (C.char ')') expressionP
        ]

intP :: Parser Int
intP = read <$> many1 C.digit

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 aP sepP =
    try $
        do
            a <- aP
            as <- many (try $ sepP >> aP)
            pure (a : as)
