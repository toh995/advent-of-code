{-# LANGUAGE NamedFieldPuns #-}

module Day20.Part2 where

import Control.Category ((>>>))
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import Data.Bifunctor (second)
import Data.Foldable (find)
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Text.Parsec (Parsec, many, many1, parse, try)
import qualified Text.Parsec.Char as C

data Tile = Tile
    { tileId :: Int
    , matrix :: Matrix Char
    }
    deriving (Eq, Show)

type Matrix a = Array (Int, Int) a

data Direction
    = North
    | South
    | East
    | West
    deriving (Bounded, Enum, Eq, Ord)

allDirections :: [Direction]
allDirections = [minBound .. maxBound]

newtype Board = Board (Map (Int, Int) Tile)

part2 :: String -> Int
part2 inputStr =
    totalHashCount - monsterHashCount
  where
    totalHashCount =
        solvedMatrix
            & A.elems
            & filter (== '#')
            & length
    monsterHashCount =
        solvedMatrix
            & inversionsRaw
            & map countMonsterHashes
            & sum
    solvedMatrix =
        board
            & mapTiles trim
            & mergeTiles
    board =
        solve tiles
            & unwrapSingleton
            & unwrapEither
    tiles =
        unwrapEither $
            parse tilesP "" inputStr

unwrapSingleton :: [a] -> Either String a
unwrapSingleton [x] = Right x
unwrapSingleton [] = Left "couldn't unwrap singleton - empty list!"
unwrapSingleton (_ : _) = Left "couldn't unwrap singleton - too many items!"

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Right b) = b
unwrapEither (Left a) = error $ show a

countMonsterHashes :: Matrix Char -> Int
countMonsterHashes matrix =
    monsters
        & concat
        & S.fromList
        & S.size
  where
    monsters =
        [ [(i + di, j + dj) | (di, dj) <- monsterCoordOffsets]
        | (i, j) <- A.indices matrix
        ]
            & filter (all $ \(i, j) -> matrix A.!? (i, j) == Just '#')

monsterCoordOffsets :: [(Int, Int)]
monsterCoordOffsets =
    [ (i, j)
    | (i, row) <- zip [0 ..] monster
    , (j, char) <- zip [0 ..] row
    , char == '#'
    ]
  where
    monster =
        [ "                  # "
        , "#    ##    ##    ###"
        , " #  #  #  #  #  #   "
        ]

trim :: Tile -> Tile
trim t@Tile{matrix} =
    t{matrix = matrix'}
  where
    matrix' =
        A.array
            ((0, 0), (tileLen - 3, tileLen - 3))
            [ ((i - 1, j - 1), char)
            | ((i, j), char) <- A.assocs matrix
            , i /= 0
            , i /= tileLen - 1
            , j /= 0
            , j /= tileLen - 1
            ]
    tileLen = len t

len :: Tile -> Int
len Tile{matrix} =
    endI - startI + 1
  where
    ((startI, _), (endI, _)) = A.bounds matrix

mergeTiles :: Board -> Matrix Char
mergeTiles (Board m) =
    A.array
        ((0, 0), rowMaxBound)
        [ (coord, char)
        | ((boardI, boardJ), Tile{matrix}) <- M.assocs m
        , ((i, j), char) <- A.assocs matrix
        , let coord = (boardI * tileLen + i, boardJ * tileLen + j)
        ]
  where
    ((maxBoardI, maxBoardJ), tile) = M.findMax m
    tileLen = len tile
    rowMaxBound =
        ( ((maxBoardI + 1) * tileLen) - 1
        , ((maxBoardJ + 1) * tileLen) - 1
        )

tileAt :: (Int, Int) -> Board -> Maybe Tile
tileAt coord (Board m) = m M.!? coord

insert :: (Int, Int) -> Tile -> Board -> Board
insert coord tile (Board m) =
    Board $
        M.insert coord tile m

mapTiles :: (Tile -> Tile) -> Board -> Board
mapTiles f (Board m) =
    Board $ M.map f m

emptyBoard :: Board
emptyBoard = Board M.empty

solve :: [Tile] -> [Board]
solve tiles =
    go
        allCoords
        unusedTilesInit
        boardInit
  where
    n = isqrt $ length tiles
    allCoords =
        [ (i, j)
        | i <- [0 .. n - 1]
        , j <- [0 .. n - 1]
        , (i, j) /= (0, 0)
        ]
    cornerTile' = fromJust $ cornerTile tiles
    unusedTilesInit =
        tiles
            & filter ((/= tileId cornerTile') . tileId)
            & concatMap inversions
    boardInit = insert (0, 0) cornerTile' emptyBoard

    go [] _ board = [board]
    go (coord : nextCoords) unusedTiles board =
        concat
            [ go nextCoords unusedTiles' board'
            | newTile <- newTiles
            , let unusedTiles' = unusedTiles & filter ((/= tileId newTile) . tileId)
            , let board' = insert coord newTile board
            ]
      where
        newTiles = filter doesMatch unusedTiles
        doesMatch = (&&) <$> doesMatchWest <*> doesMatchNorth
        doesMatchWest t =
            case westNeighbor of
                Nothing -> True
                Just westNeighbor' -> edge West t == edge East westNeighbor'
        doesMatchNorth t =
            case northNeighbor of
                Nothing -> True
                Just northNeighbor' -> edge North t == edge South northNeighbor'
        northNeighbor = tileAt (neighborCoord North coord) board
        westNeighbor = tileAt (neighborCoord West coord) board

cornerTile :: [Tile] -> Maybe Tile
cornerTile tiles =
    find isCornerTile allInversions
  where
    allInversions = concatMap inversions tiles
    isCornerTile tile =
        null (tileMatches North tile)
            && null (tileMatches West tile)
            && not (S.null southTileIds)
            && not (S.null eastTileIds)
            && not (S.size southTileIds == 1 && southTileIds == eastTileIds)
      where
        southTileIds =
            tileMatches South tile
                & map tileId
                & S.fromList
        eastTileIds =
            tileMatches East tile
                & map tileId
                & S.fromList
    tileMatches direction tile =
        tilesByEdge M.! (inverseDir direction, edge direction tile)
            & filter ((/= tileId tile) . tileId)
    tilesByEdge =
        M.fromListWith
            (++)
            [ ((dir, edge dir tile), [tile])
            | tile <- allInversions
            , dir <- allDirections
            ]

inverseDir :: Direction -> Direction
inverseDir North = South
inverseDir South = North
inverseDir East = West
inverseDir West = East

inversions :: Tile -> [Tile]
inversions t@Tile{matrix} =
    inversionsRaw matrix
        & map (\matrix' -> t{matrix = matrix'})

inversionsRaw :: Matrix a -> [Matrix a]
inversionsRaw matrix =
    rotations ++ (flipMatrix <$> rotations)
  where
    rotations =
        iterate rotate90 matrix
            & take 4

flipMatrix :: Matrix a -> Matrix a
flipMatrix matrix =
    A.ixmap
        (A.bounds matrix)
        (second (maxJ -))
        matrix
  where
    ((_, _), (_, maxJ)) = A.bounds matrix

rotate90 :: Matrix a -> Matrix a
rotate90 matrix =
    A.ixmap
        (A.bounds matrix)
        (\(i, j) -> (maxJ - j, i))
        matrix
  where
    ((_, _), (_, maxJ)) = A.bounds matrix

edge :: Direction -> Tile -> [Char]
edge dir Tile{matrix} =
    case dir of
        North -> [matrix A.! (minI, j) | j <- js]
        South -> [matrix A.! (maxI, j) | j <- js]
        East -> [matrix A.! (i, maxJ) | i <- is]
        West -> [matrix A.! (i, minJ) | i <- is]
  where
    ((minI, minJ), (maxI, maxJ)) = A.bounds matrix
    is = [minI .. maxI]
    js = [minJ .. maxJ]

neighborCoord :: Direction -> (Int, Int) -> (Int, Int)
neighborCoord North (i, j) = (i - 1, j)
neighborCoord South (i, j) = (i + 1, j)
neighborCoord East (i, j) = (i, j + 1)
neighborCoord West (i, j) = (i, j - 1)

isqrt :: (Integral a) => a -> a
isqrt =
    fromIntegral
        >>> (sqrt :: Double -> Double)
        >>> floor

type Parser = Parsec String ()

tilesP :: Parser [Tile]
tilesP = sepBy1 tileP (C.string "\n\n")

tileP :: Parser Tile
tileP = do
    _ <- C.string "Tile "
    tileId <- integralP
    _ <- C.string ":\n"
    rows <- sepBy1 rowP C.newline
    let matrix =
            A.array
                ((0, 0), (length rows - 1, length (head rows) - 1))
                [ ((i, j), char)
                | (i, row) <- zip [0 ..] rows
                , (j, char) <- zip [0 ..] row
                ]
    pure $ Tile{tileId, matrix}

integralP :: (Read a, Integral a) => Parser a
integralP = read <$> many1 (try C.digit)

rowP :: Parser [Char]
rowP = many1 (try $ C.oneOf "#.")

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 aP sepP =
    do
        a <- aP
        as <- many (try $ sepP *> aP)
        pure (a : as)
