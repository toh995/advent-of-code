module Day08.Main where

import Control.Monad
import Control.Monad.State.Lazy
import Data.Char
import Data.Functor
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.HashSet qualified as HashSet

filePath :: String
filePath = "src/Day08/data.txt"

main :: IO ()
main = do
  board <- readFile filePath <&> parseBoard . lines
  let part1Answer = part1 board
  putStrLn $ "PART 1: " ++ show part1Answer
  let part2Answer = part2 board
  putStrLn $ "PART 2: " ++ show part2Answer

part1 :: Board -> Int
part1 board =
  length $ HashSet.fromList antinodeCoords
 where
  antennaPairs :: [(Coord, Coord)]
  antennaPairs =
    concatMap pairs
      . HashMap.elems
      . coordMap
      $ board
  antinodeCoords :: [Coord]
  antinodeCoords =
    filter (isMember board)
      . catPairs
      . map antinodes1
      $ antennaPairs

part2 :: Board -> Int
part2 board =
  length $ HashSet.fromList antinodeCoords
 where
  antennaPairs :: [(Coord, Coord)]
  antennaPairs =
    concatMap pairs
      . HashMap.elems
      . coordMap
      $ board
  antinodeCoords :: [Coord]
  antinodeCoords = concatMap (antinodes2 board) antennaPairs

-- generate all possible "pairs" (i.e. C(n,2))
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a : as) = ((a,) <$> as) ++ pairs as

catPairs :: [(a, a)] -> [a]
catPairs [] = []
catPairs ((a1, a2) : rest) = a1 : a2 : catPairs rest

antinodes1 :: (Coord, Coord) -> (Coord, Coord)
antinodes1 ((i1, j1), (i2, j2)) =
  (antinode1, antinode2)
 where
  antinode1 = (i1 - di, j1 - dj)
  antinode2 = (i2 + di, j2 + dj)
  di = i2 - i1
  dj = j2 - j1

antinodes2 :: Board -> (Coord, Coord) -> [Coord]
antinodes2 board ((i1, j1), (i2, j2)) =
  nodes1 ++ nodes2
 where
  nodes1 =
    takeWhile (isMember board) $
      iterate
        (\(i, j) -> (i - di, j - dj))
        (i1, j1)
  nodes2 =
    takeWhile (isMember board) $
      iterate
        (\(i, j) -> (i + di, j + dj))
        (i2, j2)
  di = i2 - i1
  dj = j2 - j1

isMember :: Board -> Coord -> Bool
isMember Board{iMax, jMax} (i, j) =
  0 <= i
    && i <= iMax
    && 0 <= j
    && j <= jMax

data Board = Board
  { coordMap :: HashMap Char [Coord]
  , iMax, jMax :: Int
  }

type Coord = (Int, Int)

parseBoard :: [String] -> Board
parseBoard [] =
  Board
    { coordMap = HashMap.empty
    , iMax = -1
    , jMax = -1
    }
parseBoard rows@(firstRow : _) =
  Board
    { coordMap = parseCoordMap rows
    , iMax = length rows - 1
    , jMax = length firstRow - 1
    }

parseCoordMap :: [String] -> HashMap Char [Coord]
parseCoordMap rows =
  hm $
    execState (parseCoordMapS rows) initialS

data S = S
  { currCoord :: Coord
  , hm :: HashMap Char [Coord]
  }

initialS :: S
initialS =
  S
    { currCoord = (0, 0)
    , hm = HashMap.empty
    }

parseCoordMapS :: (MonadState S m) => [String] -> m ()
parseCoordMapS = mapM_ processRow

processRow :: (MonadState S m) => String -> m ()
processRow row =
  mapM_ processChar row
    >> modify (\s@S{currCoord = (i, _)} -> s{currCoord = (i + 1, 0)})

processChar :: (MonadState S m) => Char -> m ()
processChar char =
  when
    (isAlphaNum char)
    ( modify $ \s@S{hm, currCoord} ->
        let hm' =
              if HashMap.member char hm
                then HashMap.adjust (currCoord :) char hm
                else HashMap.insert char [currCoord] hm
         in s{hm = hm'}
    )
    >> modify
      (\s@S{currCoord = (i, j)} -> s{currCoord = (i, j + 1)})
