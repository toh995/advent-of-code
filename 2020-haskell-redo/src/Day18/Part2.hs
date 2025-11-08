module Day18.Part2 where

import Control.Category ((>>>))
import Data.Functor ((<&>))
import Text.Parsec (Parsec, between, choice, many, many1, parse, try)
import qualified Text.Parsec.Char as C

part2 :: String -> Int
part2 =
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
expressionP =
    sepBy1 additionTermP (C.string " * ")
        <&> product

additionTermP :: Parser Int
additionTermP =
    sepBy1 termP (C.string " + ")
        <&> sum

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
