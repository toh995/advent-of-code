module Day22.Main where

import Data.Foldable (toList)
import Data.Function ((&))
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Text.Parsec (Parsec, many, many1, try)
import qualified Text.Parsec.Char as C
import Text.Parsec.String (parseFromFile)

filePath :: String
filePath = "src/Day22/data.txt"

main :: IO ()
main = do
    (deck1, deck2) <- unwrapEither <$> parseFromFile decksP filePath
    let part1Answer = part1 deck1 deck2
    putStrLn ("PART1: " ++ show part1Answer)
    let part2Answer = part2 deck1 deck2
    putStrLn ("PART2: " ++ show part2Answer)

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Left a) = error $ show a
unwrapEither (Right b) = b

part1 :: Deck -> Deck -> Int
part1 d1 d2 =
    play1 d1 d2
        & score

part2 :: Deck -> Deck -> Int
part2 d1 d2 =
    score winnersDeck
  where
    (_, winnersDeck) = play2 d1 d2

type Deck = Seq Int

data Player = Player1 | Player2

play1 :: Deck -> Deck -> Deck
play1 Empty deck2 = deck2
play1 deck1 Empty = deck1
play1 (c1 :<| cs1) (c2 :<| cs2)
    | c1 > c2 = play1 (cs1 :|> c1 :|> c2) cs2
    | c1 < c2 = play1 cs1 (cs2 :|> c2 :|> c1)
    | otherwise = error "invalid state - no winner for the round!"

play2 :: Deck -> Deck -> (Player, Deck)
play2 deck1Init deck2Init =
    go deck1Init deck2Init S.empty
  where
    go Empty deck2 _ = (Player2, deck2)
    go deck1 Empty _ = (Player1, deck1)
    go deck1@(c1 :<| cs1) deck2@(c2 :<| cs2) seenDecks
        | (deck1, deck2) `S.member` seenDecks = (Player1, deck1)
        | otherwise =
            case roundWinner of
                Player1 -> go (cs1 :|> c1 :|> c2) cs2 seenDecks'
                Player2 -> go cs1 (cs2 :|> c2 :|> c1) seenDecks'
      where
        roundWinner
            | c1 <= Seq.length cs1 && c2 <= Seq.length cs2 = fst $ play2 (Seq.take c1 cs1) (Seq.take c2 cs2)
            | c1 > c2 = Player1
            | c2 > c1 = Player2
            | otherwise = error "invalid state - no winner for the round!"
        seenDecks' = S.insert (deck1, deck2) seenDecks

score :: Deck -> Int
score deck =
    sum
        [ cardVal * k
        | (k, cardVal) <- zip [1 ..] (reverse $ toList deck)
        ]

type Parser = Parsec String ()

decksP :: Parser (Deck, Deck)
decksP = do
    _ <- C.string "Player 1:\n"
    deck1 <- deckP
    _ <- C.string "\n\nPlayer 2:\n"
    deck2 <- deckP
    pure (deck1, deck2)

deckP :: Parser Deck
deckP =
    Seq.fromList
        <$> sepBy1 integralP C.newline

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 aP sepP = do
    a <- aP
    as <- many (try $ sepP *> aP)
    pure (a : as)

integralP :: (Read a, Integral a) => Parser a
integralP =
    read <$> many1 (try C.digit)
