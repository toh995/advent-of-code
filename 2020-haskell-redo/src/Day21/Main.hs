{-# LANGUAGE NamedFieldPuns #-}

module Day21.Main where

import Data.Function ((&))
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec (Parsec, between, many, many1, try)
import qualified Text.Parsec.Char as C
import Text.Parsec.String (parseFromFile)

filePath :: String
filePath = "src/Day21/data.txt"

main :: IO ()
main = do
    foods <- unwrapEither <$> parseFromFile foodsP filePath
    let part1Answer = part1 foods
    putStrLn ("PART1: " ++ show part1Answer)
    let part2Answer = part2 foods
    putStrLn ("PART2: " ++ part2Answer)

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Left a) = error $ show a
unwrapEither (Right b) = b

data Food = Food
    { ingredients :: [Ingredient]
    , allergens :: [Allergen]
    }

newtype Ingredient = Ingredient String
    deriving (Eq, Ord, Show)
newtype Allergen = Allergen String
    deriving (Eq, Ord, Show)

part1 :: [Food] -> Int
part1 foods =
    allIngredients
        & filter (not . (`S.member` assignedIngredients))
        & length
  where
    allIngredients = concatMap ingredients foods
    assignedIngredients =
        allergenToIngredient foods
            & M.elems
            & S.fromList

part2 :: [Food] -> String
part2 foods =
    intercalate
        ","
        [ i
        | (_, Ingredient i) <- M.toAscList $ allergenToIngredient foods
        ]

allergenToIngredient :: [Food] -> Map Allergen Ingredient
allergenToIngredient foods =
    simplify $
        M.fromListWith
            S.intersection
            [ (allergen, S.fromList (ingredients food))
            | food <- foods
            , allergen <- allergens food
            ]

simplify :: (Ord k, Ord v) => Map k (Set v) -> Map k v
simplify m
    | M.null m' = simplified
    | otherwise = simplified `M.union` simplify m'
  where
    simplified =
        m
            & M.filter ((== 1) . S.size)
            & M.map S.findMax
    singleValues = S.fromList $ M.elems simplified
    m' =
        m
            & M.filter ((> 1) . S.size)
            & M.map (`S.difference` singleValues)

type Parser = Parsec String ()

foodsP :: Parser [Food]
foodsP = sepBy1 foodP C.newline

foodP :: Parser Food
foodP = do
    ingredients <- ingredientsP
    _ <- C.char ' '
    allergens <- allergensP
    pure Food{ingredients, allergens}

ingredientsP :: Parser [Ingredient]
ingredientsP =
    sepBy1
        (Ingredient <$> wordP)
        (C.char ' ')

allergensP :: Parser [Allergen]
allergensP =
    between
        (C.string "(contains ")
        (C.char ')')
        (sepBy1 (Allergen <$> wordP) $ C.string ", ")

wordP :: Parser String
wordP = many1 $ try C.letter

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 aP sepP = do
    a <- aP
    as <- many (try $ sepP *> aP)
    pure (a : as)
