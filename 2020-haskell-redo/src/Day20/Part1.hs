{-# LANGUAGE NamedFieldPuns #-}

module Day20.Part1 where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.IntSet as IS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Parsec (Parsec, many, many1, parse, try)
import qualified Text.Parsec.Char as C

data Tile = Tile
    { tileId :: Int
    , rows :: NonEmpty (NonEmpty Char)
    }
    deriving (Show)

edges :: Tile -> [[Char]]
edges Tile{rows} =
    NE.toList
        <$> [topRow, bottomRow, leftCol, rightCol]
  where
    topRow = NE.head rows
    bottomRow = NE.last rows
    leftCol = NE.head cols
    rightCol = NE.last cols
    cols = NE.transpose rows

splitOn :: String -> String -> [String]
splitOn delim =
    T.pack
        >>> T.splitOn (T.pack delim)
        >>> map T.unpack

part1 :: String -> Int
part1 inputStr =
    tiles
        & filter isCornerTile
        & map tileId
        & product
  where
    tiles =
        parse tilesP "" inputStr
            & unwrapEither
    edgeToIds =
        M.fromListWith
            IS.union
            [ (edge, IS.singleton $ tileId tile)
            | tile <- tiles
            , edge <- edges tile ++ map reverse (edges tile)
            ]
    isCornerTile tile =
        tile
            & edges
            & filter
                ( \edge ->
                    let ids =
                            edgeToIds
                                M.! edge
                                & IS.delete (tileId tile)
                     in not $ IS.null ids
                )
            & ((== 2) . length)

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Right b) = b
unwrapEither (Left a) = error $ show a

type Parser = Parsec String ()

tilesP :: Parser [Tile]
tilesP = sepBy1 tileP (C.string "\n\n")

tileP :: Parser Tile
tileP = do
    _ <- C.string "Tile "
    tileId <- intP
    _ <- C.string ":\n"
    rows <- sepBy1 rowP C.newline <&> NE.fromList
    pure $ Tile{tileId, rows}

intP :: Parser Int
intP = read <$> many1 (try C.digit)

rowP :: Parser (NonEmpty Char)
rowP =
    NE.fromList <$> many1 (try $ C.oneOf "#.")

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 aP sepP = do
    a <- aP
    as <- many (try $ sepP >> aP)
    pure (a : as)
