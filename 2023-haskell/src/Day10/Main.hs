module Day10.Main where

import Control.Monad.Trans.State.Lazy
import Data.Functor
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Maybe
import Safe

type I = Int
type J = Int
type Coord = (I, J)
type PrevCoord = Coord
type CurrCoord = Coord
type Position = (PrevCoord, CurrCoord)

data Matrix a = Matrix
  { iMax, jMax :: Int
  , hm :: HashMap Coord a
  }
  deriving (Eq, Show)

type Tile = Char

data Direction = N | S | E | W

-- Matrix primitives
buildMatrix :: [[a]] -> Matrix a
buildMatrix [] = error "buildMatrix: empty list."
buildMatrix rows@(r : _) =
  Matrix
    { iMax = length rows - 1
    , jMax = length r - 1
    , hm =
        foldr
          insertRow
          HashMap.empty
          (zip rows [0 ..])
    }
 where
  insertRow (row, i) hm =
    foldr
      (\(x, j) hm' -> HashMap.insert (i, j) x hm')
      hm
      (zip row [0 ..])

coordsForVal :: (Eq a) => a -> Matrix a -> [Coord]
coordsForVal a m =
  filter ((== Just a) . lookupM m)
    . HashMap.keys
    . hm
    $ m

lookupM :: Matrix a -> Coord -> Maybe a
lookupM (Matrix{hm}) =
  flip HashMap.lookup hm

filterCoords :: (Coord -> Bool) -> Matrix Tile -> Matrix Tile
filterCoords p m =
  m
    { hm =
        HashMap.filterWithKey
          (\coord _ -> p coord)
          (hm m)
    }

expand :: Matrix a -> Matrix a
expand (Matrix{hm, iMax, jMax}) =
  Matrix
    { hm = HashMap.mapKeys (\(i, j) -> (i * 2 + 1, j * 2 + 1)) hm
    , iMax = iMax * 2 + 2
    , jMax = jMax * 2 + 2
    }

expandCoords :: Matrix Tile -> HashSet Coord -> HashSet Coord
expandCoords matrix =
  foldMap f
    . HashSet.map
      (\(i, j) -> (i * 2 + 1, j * 2 + 1))
 where
  f (i, j) =
    case lookupM matrix (i, j) of
      Just '|' -> HashSet.fromList [(i, j), (i - 1, j), (i + 1, j)]
      Just '-' -> HashSet.fromList [(i, j), (i, j - 1), (i, j + 1)]
      Just 'L' -> HashSet.fromList [(i, j), (i - 1, j), (i, j + 1)]
      Just 'J' -> HashSet.fromList [(i, j), (i - 1, j), (i, j - 1)]
      Just '7' -> HashSet.fromList [(i, j), (i, j - 1), (i + 1, j)]
      Just 'F' -> HashSet.fromList [(i, j), (i, j + 1), (i + 1, j)]
      Just 'S' ->
        HashSet.fromList $
          (i, j)
            : mapMaybe
              g
              [(i - 2, j), (i + 2, j), (i, j - 2), (i, j + 2)]
      _ -> HashSet.fromList [(i, j)]
   where
    g coord = do
      otherTile <- lookupM matrix coord
      if
        | coord == (i - 2, j) && otherTile `elem` ['|', '7', 'F'] -> Just (i - 1, j)
        | coord == (i + 2, j) && otherTile `elem` ['|', 'L', 'J'] -> Just (i + 1, j)
        | coord == (i, j - 2) && otherTile `elem` ['-', 'L', 'F'] -> Just (i, j - 1)
        | coord == (i, j + 2) && otherTile `elem` ['-', 'J', '7'] -> Just (i, j + 1)
        | otherwise -> Nothing

compressCoords :: [Coord] -> [Coord]
compressCoords =
  filter
    ((&&) <$> odd . fst <*> odd . snd)

isValidCoord :: Matrix a -> Coord -> Bool
isValidCoord Matrix{iMax, jMax} (i, j) =
  (0 <= i && i <= iMax)
    && (0 <= j && j <= jMax)

allCoords :: Matrix a -> [Coord]
allCoords Matrix{iMax, jMax} =
  (,) <$> [0 .. iMax] <*> [0 .. jMax]

neighborCoords :: Matrix a -> Coord -> [Coord]
neighborCoords m (i, j) =
  filter
    (isValidCoord m)
    [ (i + 1, j)
    , (i - 1, j)
    , (i, j + 1)
    , (i, j - 1)
    ]

-- Main logic
filePath :: String
filePath = "src/Day10/data.txt"

main :: IO ()
main = do
  matrix <- readFile filePath <&> buildMatrix . lines

  let part1Answer = part1 matrix
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 matrix
  putStrLn $ "PART 2: " ++ show part2Answer

part1 :: Matrix Tile -> Maybe Int
part1 matrix = do
  startPos <- getStartPos matrix
  let path = computePath matrix startPos
  let cycleLen =
        length
          . takeWhile (/= startPos)
          . drop 1
          $ path
  pure $ (cycleLen + 1) `div` 2

part2 :: Matrix Tile -> Maybe Int
part2 matrix = do
  pathCoords <- getPathCoords matrix
  let matrix' =
        expand
          . filterCoords (`HashSet.member` pathCoords)
          $ matrix
  let pathCoords' = expandCoords matrix' pathCoords
  let allCoords' = HashSet.fromList . allCoords $ matrix'
  let outsideCoords' = getContiguousCoords matrix' pathCoords' (0, 0)
  let insideCoords' =
        HashSet.difference
          allCoords'
          $ HashSet.union outsideCoords' pathCoords'
  pure (length . compressCoords . HashSet.toList $ insideCoords')

getPathCoords :: Matrix Tile -> Maybe (HashSet Coord)
getPathCoords matrix = do
  startPos <- getStartPos matrix
  let infPath = computePath matrix startPos
  let path = startPos : (takeWhile (/= startPos) . drop 1 $ infPath)
  let pathCoords = HashSet.fromList $ map fst path
  pure pathCoords

getStartPos :: Matrix Tile -> Maybe Position
getStartPos matrix = do
  sCoord <- headMay $ coordsForVal 'S' matrix
  let startPoses =
        filter (isLegalPos matrix)
          . map (sCoord,)
          $ neighborCoords matrix sCoord
  headMay startPoses

computePath :: Matrix Tile -> Position -> [Position]
computePath matrix startPos =
  startPos : rest
 where
  rest = fromMaybe [] m
  m = do
    next <- nextPos matrix startPos
    pure $ computePath matrix next

nextPos :: Matrix Tile -> Position -> Maybe Position
nextPos matrix (prevCoord, currCoord) =
  headMay
    . filter (isLegalPos matrix)
    . map (currCoord,)
    . filter (/= prevCoord)
    $ neighborCoords matrix currCoord

isLegalPos :: Matrix Tile -> Position -> Bool
isLegalPos matrix p@(prevCoord, currCoord) =
  fromMaybe False m
 where
  m = do
    dir <- getDir p
    prevTile <- lookupM matrix prevCoord
    currTile <- lookupM matrix currCoord
    case dir of
      N -> pure $ prevTile `elem` ['S', '|', 'L', 'J'] && currTile `elem` ['S', '|', '7', 'F']
      S -> pure $ prevTile `elem` ['S', '|', '7', 'F'] && currTile `elem` ['S', '|', 'L', 'J']
      E -> pure $ prevTile `elem` ['S', '-', 'L', 'F'] && currTile `elem` ['S', '-', 'J', '7']
      W -> pure $ prevTile `elem` ['S', '-', 'J', '7'] && currTile `elem` ['S', '-', 'L', 'F']

getDir :: Position -> Maybe Direction
getDir p
  | (prevI - 1, prevJ) == (currI, currJ) = Just N
  | (prevI + 1, prevJ) == (currI, currJ) = Just S
  | (prevI, prevJ - 1) == (currI, currJ) = Just W
  | (prevI, prevJ + 1) == (currI, currJ) = Just E
  | otherwise = Nothing
 where
  ((prevI, prevJ), (currI, currJ)) = p

getContiguousCoords :: Matrix Tile -> HashSet Coord -> Coord -> HashSet Coord
getContiguousCoords matrix pathCoords startCoord =
  execState (f startCoord) HashSet.empty
 where
  f :: Coord -> State (HashSet Coord) ()
  f coord = do
    seen <- get
    put $ HashSet.insert coord seen
    let neighbors =
          filter (not . (`HashSet.member` pathCoords))
            . filter (not . (`HashSet.member` seen))
            . neighborCoords matrix
            $ coord
    mapM_ f neighbors
