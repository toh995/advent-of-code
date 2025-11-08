{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day16.Main where

import Data.Function ((&))
import Data.List (isPrefixOf, transpose)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec (Parsec, many, many1, manyTill, try)
import qualified Text.Parsec.Char as C
import Text.Parsec.String (parseFromFile)

data Field = Field
    { name :: String
    , range1 :: (Int, Int)
    , range2 :: (Int, Int)
    }
    deriving (Eq, Ord, Show)

type Ticket = [Int]

filePath :: String
filePath = "src/Day16/data.txt"

main :: IO ()
main = do
    parseResult <- parseFromFile allP filePath
    let (fields, yourTicket, nearbyTickets) =
            case parseResult of
                Right val -> val
                Left err -> error $ show err
    let part1Answer = part1 fields nearbyTickets
    putStrLn ("PART 1: " ++ show part1Answer)
    let part2Answer = part2 fields nearbyTickets yourTicket
    putStrLn ("PART 2: " ++ show part2Answer)

part1 :: [Field] -> [Ticket] -> Int
part1 fields nearbyTickets =
    concat nearbyTickets
        & filter (not . isValidTicketVal fields)
        & sum

part2 :: [Field] -> [Ticket] -> Ticket -> Int
part2 fields nearbyTickets yourTicket =
    indexes
        & map (yourTicket !!)
        & product
  where
    indexes =
        fieldNameToIndex fields validTickets
            & M.filterWithKey (\key _ -> "departure" `isPrefixOf` key)
            & M.elems
    validTickets =
        filter
            (isValidTicket fields)
            (yourTicket : nearbyTickets)

isValidTicket :: [Field] -> Ticket -> Bool
isValidTicket fields ticket = all (isValidTicketVal fields) ticket

isValidTicketVal :: [Field] -> Int -> Bool
isValidTicketVal fields n = any (isInRange n) fields

isInRange :: Int -> Field -> Bool
isInRange n (Field{range1 = (s1, e1), range2 = (s2, e2)}) =
    (s1 <= n && n <= e1)
        || (s2 <= n && n <= e2)

fieldNameToIndex :: [Field] -> [Ticket] -> Map String Int
fieldNameToIndex fields tickets =
    simplify fieldNameToIndexes
  where
    fieldNameToIndexes =
        M.fromList
            [ ( name field
              , [ i
                | (i, ticketVals) <- zip [0 ..] transposedTix
                , all (`isInRange` field) ticketVals
                ]
              )
            | field <- fields
            ]
    transposedTix = transpose tickets

simplify :: (Ord a, Ord b, Show a, Show b) => Map a [b] -> Map a b
simplify mInit =
    go
        queueInit
        mInit'
        M.empty
  where
    mInit' = M.map S.fromList mInit
    queueInit =
        [ (key, S.findMin set)
        | (key, set) <- M.toList mInit'
        , S.size set == 1
        ]
    go [] m result
        | M.null m = result
        | otherwise = error "could not simplify"
    go ((key, b) : queue) im result =
        let
            result' = M.insert key b result
            deductKeys =
                [ key'
                | (key', set) <- M.toList im
                , key' /= key
                , b `S.member` set
                ]
            im' =
                foldr
                    (M.adjust $ S.delete b)
                    im
                    deductKeys
                    & M.delete key
            queue' =
                [ (key', S.findMin set')
                | key' <- deductKeys
                , let set' = im' M.! key'
                , S.size set' == 1
                ]
                    ++ queue
         in
            go queue' im' result'

type Parser = Parsec String ()

allP :: Parser ([Field], Ticket, [Ticket])
allP = do
    fields <- fieldsP
    _ <- C.newline >> C.newline
    yourTicket <- yourTicketP
    _ <- C.newline >> C.newline
    nearbyTickets <- nearbyTicketsP
    pure (fields, yourTicket, nearbyTickets)

fieldsP :: Parser [Field]
fieldsP = sepBy1 fieldRangeP C.newline

fieldRangeP :: Parser Field
fieldRangeP = do
    name <- manyTill C.anyChar (C.string ": ")
    range1 <- intPairP
    _ <- C.string " or "
    range2 <- intPairP
    pure Field{name, range1, range2}

intPairP :: Parser (Int, Int)
intPairP = do
    i1 <- intP
    _ <- C.char '-'
    i2 <- intP
    pure (i1, i2)

intP :: Parser Int
intP = read <$> many1 C.digit

yourTicketP :: Parser Ticket
yourTicketP =
    C.string "your ticket:\n"
        >> ticketP

nearbyTicketsP :: Parser [Ticket]
nearbyTicketsP =
    C.string "nearby tickets:\n"
        >> sepBy1 ticketP C.newline

ticketP :: Parser Ticket
ticketP = sepBy1 intP (C.char ',')

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sepP = do
    first <- p
    rest <- many (try $ sepP >> p)
    return (first : rest)
