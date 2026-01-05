module Day24.Part01 where

import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Text.Parsec (Parsec, many, many1, option, parse, try)
import Text.Parsec.Char qualified as C

data Hailstone = Hailstone
    { position :: (Integer, Integer, Integer)
    , velocity :: (Integer, Integer, Integer)
    }

part1 :: String -> Int
part1 inputStr =
    hailstones
        & pairs
        & mapMaybe (uncurry solve)
        & filter isValidSolution
        & length
  where
    hailstones =
        parse hailstonesP "" inputStr
            & unwrapEither
    isValidSolution ((x, y), time1, time2) =
        time1 >= 0
            && time2 >= 0
            && isInRange x
            && isInRange y
    isInRange n = 200000000000000 <= n && n <= 400000000000000

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Left a) = error $ show a
unwrapEither (Right b) = b

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = ((x,) <$> xs) ++ pairs xs

solve :: Hailstone -> Hailstone -> Maybe ((Rational, Rational), Rational, Rational)
solve h1 h2 = do
    let (x1, y1, _) = mapTup3 toRational $ position h1
    let (x2, y2, _) = mapTup3 toRational $ position h2
    let (dx1, dy1, _) = mapTup3 toRational $ velocity h1
    let (dx2, dy2, _) = mapTup3 toRational $ velocity h2
    x <- ((dy1 / dx1 * x1) - (dy2 / dx2 * x2) - y1 + y2) `safeDiv` ((dy1 / dx1) - (dy2 / dx2))
    let y = dy1 * ((x / dx1) - (x1 / dx1) + (y1 / dy1))
    let time1 = (x / dx1) - (x1 / dx1)
    let time2 = (x / dx2) - (x2 / dx2)
    pure ((x, y), time1, time2)

mapTup3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTup3 f (x, y, z) = (f x, f y, f z)

safeDiv :: (Fractional a, Eq a) => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just $ x / y

type Parser = Parsec String ()

hailstonesP :: Parser [Hailstone]
hailstonesP = sepEndBy1 hailstoneP C.newline

hailstoneP :: Parser Hailstone
hailstoneP = do
    let sepP = C.char ',' >> C.spaces
    x <- integerP <* sepP
    y <- integerP <* sepP
    z <- integerP
    _ <- C.string " @ "
    dx <- integerP <* sepP
    dy <- integerP <* sepP
    dz <- integerP
    pure
        Hailstone
            { position = (x, y, z)
            , velocity = (dx, dy, dz)
            }

integerP :: Parser Integer
integerP = do
    leadingNegative <- option "" (try $ C.string "-")
    digits <- many1 $ try C.digit
    let numStr = leadingNegative ++ digits
    pure $ read numStr

sepEndBy1 :: Parser a -> Parser sep -> Parser [a]
sepEndBy1 aP sepP = do
    a <- aP
    as <- many (try $ sepP >> aP)
    pure (a : as)
