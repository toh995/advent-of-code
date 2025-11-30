module Day18.Part02 where

import Data.Char (digitToInt)
import Data.Function ((&))
import Data.Functor ((<&>))
import Text.Parsec (Parsec, between, many, many1, try)
import Text.Parsec.Char qualified as C
import Text.Parsec.String (parseFromFile)

filePath :: String
filePath = "src/Day18/data.txt"

main :: IO ()
main = do
    commands <-
        parseFromFile linesP filePath
            <&> unwrapEither
    let part2Answer = part2 commands
    putStrLn $ "PART2: " ++ show part2Answer

part2 :: [Command] -> Int
part2 commands =
    perimeter' + innerCoordCount
  where
    perimeter' = perimeter commands
    innerCoordCount = area' - (perimeter' `div` 2) + 1 -- via pick's theorem
    area' = area commands

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Left a) = error $ show a
unwrapEither (Right b) = b

vertices :: [Command] -> [(Int, Int)]
vertices = scanl (flip doCommand) (0, 0)

doCommand :: Command -> (Int, Int) -> (Int, Int)
doCommand Command{direction, magnitude} (x, y) =
    case direction of
        R -> (x + magnitude, y)
        D -> (x, y - magnitude)
        L -> (x - magnitude, y)
        U -> (x, y + magnitude)

perimeter :: [Command] -> Int
perimeter = sum . map magnitude

-- Use shoelace formula
area :: [Command] -> Int
area =
    abs
        . (`div` 2)
        . sum
        . map (\((_, y0), (x1, _), (_, y2)) -> x1 * (y2 - y0))
        . triples
        . vertices

triples :: [a] -> [(a, a, a)]
triples xs =
    go $ xs ++ take 2 xs
  where
    go (x : y : z : rest) = (x, y, z) : go (y : z : rest)
    go _ = []

data Command = Command
    { direction :: Direction
    , magnitude :: Int
    }
    deriving (Eq, Show)

data Direction = R | D | L | U
    deriving (Eq, Show)

type Parser = Parsec String ()

linesP :: Parser [Command]
linesP = sepBy1 lineP C.newline

lineP :: Parser Command
lineP =
    C.letter
        >> C.char ' '
        >> intP
        >> C.char ' '
        >> between (C.char '(') (C.char ')') commandP

commandP :: Parser Command
commandP = do
    _ <- C.char '#'
    s <- many1 (try C.hexDigit)
    let magnitude =
            init s
                & hexStringToInt
    let direction =
            last s
                & digitToInt
                & intToDirection
    pure Command{direction, magnitude}

intP :: Parser Int
intP = read <$> many1 (try C.digit)

sepBy1 :: Parser a -> Parser end -> Parser [a]
sepBy1 aP sepP = do
    a <- aP
    as <- many (try $ sepP >> aP)
    pure (a : as)

hexStringToInt :: String -> Int
hexStringToInt =
    foldl
        (\acc char -> (acc * 16) + digitToInt char)
        0

intToDirection :: Int -> Direction
intToDirection 0 = R
intToDirection 1 = D
intToDirection 2 = L
intToDirection 3 = U
intToDirection n = error $ "could not convert int to direction - invalid integer " ++ show n
