module Day10.Part2 where

import Control.Category ((>>>))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (tails)
import Data.Maybe (fromJust)
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import Text.Parsec (Parsec, between, many, many1, parse, try)
import qualified Text.Parsec.Char as C

-- part2 :: String -> Int
-- part2 = _
--
-- unwrapEither :: (Show a) => Either a b -> b
-- unwrapEither (Left a) = error $ show a
-- unwrapEither (Right b) = b
--
-- type Parser = Parsec String ()
--
-- rowsP :: Parser [(LightState, [Button])]
-- rowsP = sepBy1 rowP C.newline
--
-- rowP :: Parser (LightState, [Button])
-- rowP = do
--     targetLights <- targetLightsP
--     _ <- C.char ' '
--     buttons <- sepBy1 buttonP (C.char ' ')
--     _ <- C.char ' '
--     _ <-
--         between
--             (C.char '{')
--             (C.char '}')
--             (sepBy1 intP (C.char ','))
--     pure (targetLights, buttons)
--
-- targetLightsP :: Parser LightState
-- targetLightsP = do
--     _ <- C.char '['
--     str <- many1 (try $ C.oneOf "#.")
--     _ <- C.char ']'
--     pure $
--         IS.fromList
--             [ i
--             | (char, i) <- zip str [0 ..]
--             , char == '#'
--             ]
--
-- buttonP :: Parser Button
-- buttonP =
--     between
--         (C.char '(')
--         (C.char ')')
--         (Button <$> sepBy1 intP (C.char ','))
--
-- intP :: Parser Int
-- intP = read <$> many1 (try C.digit)
--
-- sepBy1 :: Parser a -> Parser sep -> Parser [a]
-- sepBy1 aP sepP = do
--     a <- aP
--     as <- many (try $ sepP >> aP)
--     pure (a : as)
