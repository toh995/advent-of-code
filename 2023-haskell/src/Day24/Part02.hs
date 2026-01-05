module Day24.Part02 where

import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Ratio (Ratio, denominator, numerator)
import Text.Parsec (Parsec, many, many1, option, parse, try)
import Text.Parsec.Char qualified as C

data Hailstone = Hailstone
    { position :: (Rational, Rational, Rational)
    , velocity :: (Rational, Rational, Rational)
    }

-- Based on https://www.reddit.com/r/adventofcode/comments/18pnycy/comment/kxqjg33/
part2 :: String -> Integer
part2 inputStr =
    fromJust
        . ratioToIntegral
        $ p `dot` (1, 1, 1)
  where
    (h0, h1, h2) =
        parse hailstonesP "" inputStr
            & unwrapEither
            & take3
            & fromJust
    p1 = position h1 `vSub` position h0
    v1 = velocity h1 `vSub` velocity h0
    p2 = position h2 `vSub` position h0
    v2 = velocity h2 `vSub` velocity h0
    t1 = negate $ ((p1 `cross` p2) `dot` v2) / ((v1 `cross` p2) `dot` v2)
    t2 = negate $ ((p1 `cross` p2) `dot` v1) / ((p1 `cross` v2) `dot` v1)
    c1 = position h1 `vAdd` (t1 `scalarMult` velocity h1)
    c2 = position h2 `vAdd` (t2 `scalarMult` velocity h2)
    v = (1 / (t2 - t1)) `scalarMult` (c2 `vSub` c1)
    p = c1 `vSub` (t1 `scalarMult` v)

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Left a) = error $ show a
unwrapEither (Right b) = b

take3 :: [a] -> Maybe (a, a, a)
take3 (x : y : z : _) = Just (x, y, z)
take3 _ = Nothing

ratioToIntegral :: (Integral a) => Ratio a -> Maybe a
ratioToIntegral x
    | denominator x == 1 = Just $ numerator x
    | otherwise = Nothing

vSub :: (Num a) => (a, a, a) -> (a, a, a) -> (a, a, a)
vSub (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

vAdd :: (Num a) => (a, a, a) -> (a, a, a) -> (a, a, a)
vAdd (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

scalarMult :: (Num a) => a -> (a, a, a) -> (a, a, a)
scalarMult s (x, y, z) = (s * x, s * y, s * z)

dot :: (Num a) => (a, a, a) -> (a, a, a) -> a
dot (x1, y1, z1) (x2, y2, z2) =
    sum
        [ x1 * x2
        , y1 * y2
        , z1 * z2
        ]

cross :: (Num a) => (a, a, a) -> (a, a, a) -> (a, a, a)
cross (x1, y1, z1) (x2, y2, z2) =
    ( y1 * z2 - z1 * y2
    , z1 * x2 - x1 * z2
    , x1 * y2 - y1 * x2
    )

type Parser = Parsec String ()

hailstonesP :: Parser [Hailstone]
hailstonesP = sepEndBy1 hailstoneP C.newline

hailstoneP :: Parser Hailstone
hailstoneP = do
    let sepP = C.char ',' >> C.spaces
    x <- fromIntegral <$> (integerP <* sepP)
    y <- fromIntegral <$> (integerP <* sepP)
    z <- fromIntegral <$> integerP
    _ <- C.string " @ "
    dx <- fromIntegral <$> (integerP <* sepP)
    dy <- fromIntegral <$> (integerP <* sepP)
    dz <- fromIntegral <$> integerP
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
