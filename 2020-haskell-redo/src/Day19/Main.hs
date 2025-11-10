module Day19.Main where

import Control.Applicative ((<|>))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (tails)
import Data.Map (Map)
import qualified Data.Map as M
import Text.Parsec (Parsec, between, many, many1, try)
import qualified Text.Parsec.Char as C
import Text.Parsec.String (parseFromFile)

type RuleId = String

data Rule
    = SingleChar Char
    | Or [[RuleId]]

filePath :: String
filePath = "src/Day19/data.txt"

main :: IO ()
main = do
    (rulesById, messages) <- parseFromFile inputP filePath <&> unwrapEither

    let part1Answer = part1 rulesById messages
    putStrLn ("PART1: " ++ show part1Answer)
    let part2Answer = part2 rulesById messages
    putStrLn ("PART2: " ++ show part2Answer)

part1 :: Map RuleId Rule -> [String] -> Int
part1 rulesById messages =
    messages
        & filter doesMatch
        & length
  where
    doesMatch msg =
        matchMemo M.! ("0", msg)
            & filter (== "")
            & (not . null)

    matchMemo =
        M.fromList
            [ ((ruleId, subMsg), match rule subMsg)
            | (ruleId, rule) <- M.toList rulesById
            , subMsg <- concatMap tails messages
            ]

    match (SingleChar _) "" = []
    match (SingleChar char) (c : cs)
        | char == c = [cs]
        | otherwise = []
    match (Or ruleIdss) s = concatMap (matchSeq s) ruleIdss

    matchSeq s [] = [s]
    matchSeq s (ruleId : ruleIds) =
        concat
            [ matchSeq s' ruleIds
            | s' <- matchMemo M.! (ruleId, s)
            ]

part2 :: Map RuleId Rule -> [String] -> Int
part2 rulesById = part1 rulesById'
  where
    rulesById' =
        rulesById
            & M.insert "8" (Or [["42"], ["42", "8"]])
            & M.insert "11" (Or [["42", "31"], ["42", "11", "31"]])

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Right val) = val
unwrapEither (Left err) = error $ show err

type Parser = Parsec String ()

inputP :: Parser (Map RuleId Rule, [String])
inputP = do
    kvPairs <- sepBy1 ruleLineP C.newline
    let rulesById = M.fromList kvPairs
    _ <- C.string "\n\n"
    messages <- sepBy1 messageP C.newline
    pure (rulesById, messages)

ruleLineP :: Parser (RuleId, Rule)
ruleLineP =
    (,)
        <$> (ruleIdP <* C.string ": ")
        <*> ruleP

ruleIdP :: Parser String
ruleIdP = many1 $ try C.digit

ruleP :: Parser Rule
ruleP =
    try singleCharRuleP
        <|> try orRuleP

singleCharRuleP :: Parser Rule
singleCharRuleP =
    SingleChar
        <$> between (C.char '"') (C.char '"') (C.anyChar)

orRuleP :: Parser Rule
orRuleP =
    Or
        <$> sepBy1 ruleIdsP (C.string " | ")
  where
    ruleIdsP = sepBy1 ruleIdP (C.char ' ')

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 aP sepP = do
    a <- aP
    as <- many (try $ sepP >> aP)
    pure (a : as)

messageP :: Parser String
messageP = many1 (try $ C.noneOf "\n")
