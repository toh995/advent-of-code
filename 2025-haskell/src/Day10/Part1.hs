module Day10.Part1 where

import Control.Category ((>>>))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (tails)
import Data.Maybe (fromJust)
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import Text.Parsec (Parsec, between, many, many1, parse, try)
import qualified Text.Parsec.Char as C

part1 :: String -> Int
part1 =
    parse rowsP ""
        >>> unwrapEither
        >>> map (fromJust . uncurry fewestButtonPresses)
        >>> sum

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Left a) = error $ show a
unwrapEither (Right b) = b

type LightState = IntSet

newtype Button = Button {indexes :: [Int]}
    deriving (Eq, Ord, Show)

fewestButtonPresses :: LightState -> [Button] -> Maybe Int
fewestButtonPresses targetLights allButtons =
    go $
        Seq.fromList [(startLights, allButtons, 0)]
  where
    startLights = IS.empty
    go Empty = Nothing
    go ((currLights, availableButtons, pressCount) :<| queue)
        | currLights == targetLights = Just pressCount
        | otherwise =
            go $
                queue
                    >< Seq.fromList
                        [ (nextLights, otherButtons, pressCount + 1)
                        | (button : otherButtons) <- tails availableButtons
                        , let nextLights = pressButton button currLights
                        ]

pressButton :: Button -> LightState -> LightState
pressButton button lights =
    foldr
        ( \i acc ->
            if i `IS.member` acc
                then IS.delete i acc
                else IS.insert i acc
        )
        lights
        (indexes button)

type Parser = Parsec String ()

rowsP :: Parser [(LightState, [Button])]
rowsP = sepBy1 rowP C.newline

rowP :: Parser (LightState, [Button])
rowP = do
    targetLights <- targetLightsP
    _ <- C.char ' '
    buttons <- sepBy1 buttonP (C.char ' ')
    _ <- C.char ' '
    _ <-
        between
            (C.char '{')
            (C.char '}')
            (sepBy1 intP (C.char ','))
    pure (targetLights, buttons)

targetLightsP :: Parser LightState
targetLightsP = do
    _ <- C.char '['
    str <- many1 (try $ C.oneOf "#.")
    _ <- C.char ']'
    pure $
        IS.fromList
            [ i
            | (char, i) <- zip str [0 ..]
            , char == '#'
            ]

buttonP :: Parser Button
buttonP =
    between
        (C.char '(')
        (C.char ')')
        (Button <$> sepBy1 intP (C.char ','))

intP :: Parser Int
intP = read <$> many1 (try C.digit)

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 aP sepP = do
    a <- aP
    as <- many (try $ sepP >> aP)
    pure (a : as)
