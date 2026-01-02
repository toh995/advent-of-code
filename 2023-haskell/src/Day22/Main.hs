module Day22.Main where

import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as S
import Text.Parsec (Parsec, many, many1, try)
import Text.Parsec.Char qualified as C
import Text.Parsec.String (parseFromFile)

filePath :: FilePath
filePath = "src/Day22/data.txt"

main :: IO ()
main = do
    bricks <-
        parseFromFile bricksP filePath
            <&> unwrapEither
    let part1Answer = part1 bricks
    putStrLn $ "PART1: " ++ show part1Answer
    let part2Answer = part2 bricks
    putStrLn $ "PART 2: " ++ show part2Answer

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Left a) = error $ show a
unwrapEither (Right b) = b

part1 :: [Brick] -> Int
part1 bricks =
    length $
        filter canDisintegrate bricks
  where
    Graph{supportedBy, supports} = newGraph bricks
    canDisintegrate Brick{brickId} =
        all
            ( \topId ->
                case supportedBy M.!? topId of
                    Just (_ : _ : _) -> True
                    _ -> False
            )
            topIds
      where
        topIds =
            fromMaybe [] (supports M.!? brickId)

part2 :: [Brick] -> Int
part2 bricks =
    sum $
        map countFallingBricks bricks
  where
    Graph{supportedBy, supports} = newGraph bricks
    indegreesInit = M.map length supportedBy

    countFallingBricks brick =
        (-1)
            + go
                [brickId brick]
                indegreesInit
                0
      where
        go [] _ count = count
        go (zeroDegreeId : zeroDegreeIds) currIndegrees count =
            go
                (newZeroDegreeIds ++ zeroDegreeIds)
                nextIndegrees
                (count + 1)
          where
            outNeighbors = M.findWithDefault [] zeroDegreeId supports
            newZeroDegreeIds =
                filter
                    ((== Just 0) . (nextIndegrees M.!?))
                    outNeighbors
            nextIndegrees =
                foldr
                    (M.adjust (subtract 1))
                    currIndegrees
                    outNeighbors

newGraph :: [Brick] -> Graph
newGraph bricks =
    Graph{supports, supportedBy}
  where
    supports =
        M.map toList $
            M.fromListWith
                S.union
                [(belowId, S.singleton topId) | (belowId, topId) <- supportsEdges bricks]
    supportedBy =
        M.fromListWith
            (++)
            [ (topId, [belowId])
            | (belowId, topIds) <- M.assocs supports
            , topId <- topIds
            ]

supportsEdges :: [Brick] -> [(BrickId, BrickId)]
supportsEdges bricks =
    go
        sortedBricks
        M.empty
        M.empty
        []
  where
    sortedBricks =
        sortOn
            ( \brick ->
                let (_, _, z1) = coord1 brick
                    (_, _, z2) = coord2 brick
                 in (min z1 z2, max z1 z2)
            )
            bricks

    go [] _ _ edges = edges
    go (brick : otherBricks) highestZs highestIds edges =
        go
            otherBricks
            nextHighestZs
            nextHighestIds
            (newEdges ++ edges)
      where
        nextHighestZs =
            foldr
                (\(x, y) -> M.insert (x, y) nextZ)
                highestZs
                brickXYs
        nextZ = belowZ + abs (z1 - z2) + 1
        nextHighestIds =
            foldr
                (\(x, y) -> M.insert (x, y) (brickId brick))
                highestIds
                brickXYs
        newEdges = [(belowId, brickId brick) | belowId <- belowIds]
        belowIds =
            catMaybes $
                [ highestIds M.!? (x, y)
                | (x, y) <- brickXYs
                , highestZs M.!? (x, y) == Just belowZ
                ]
        belowZ = maximum belowZs
        belowZs =
            [ M.findWithDefault 0 (x, y) highestZs
            | (x, y) <- brickXYs
            ]
        brickXYs =
            [ (x, y)
            | x <- [min x1 x2 .. max x1 x2]
            , y <- [min y1 y2 .. max y1 y2]
            ]
        (x1, y1, z1) = coord1 brick
        (x2, y2, z2) = coord2 brick

type Coord = (Int, Int, Int)

type BrickId = Int

data Brick = Brick
    { brickId :: BrickId
    , coord1 :: Coord
    , coord2 :: Coord
    }

data Graph = Graph
    { supportedBy :: Map BrickId [BrickId]
    , supports :: Map BrickId [BrickId]
    }

type Parser = Parsec String ()

bricksP :: Parser [Brick]
bricksP =
    sepBy1 brickRawP C.newline
        <&> \blocksRaw ->
            [ Brick{brickId, coord1, coord2}
            | (brickId, (coord1, coord2)) <- zip [0 ..] blocksRaw
            ]

brickRawP :: Parser (Coord, Coord)
brickRawP = do
    coord1 <- coordP
    _ <- C.char '~'
    coord2 <- coordP
    pure (coord1, coord2)

coordP :: Parser Coord
coordP = do
    x <- intP
    _ <- C.char ','
    y <- intP
    _ <- C.char ','
    z <- intP
    pure (x, y, z)

intP :: Parser Int
intP = read <$> many1 (try C.digit)

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 aP sepP = do
    a <- aP
    as <- many (try $ sepP >> aP)
    pure (a : as)
