module Day18 where

import Data.Composition
import Data.Either
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

----------------------
-- SECTION: Main IO --
----------------------
filePath :: String
filePath = "data/Day18.txt"

main :: IO ()
main = do
  lavaCoords <- parseLavaCoords <$> readFile filePath

  let part1Answer = part1 lavaCoords
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 lavaCoords
  putStrLn $ "PART 2: " ++ show part2Answer

----------------------
-- SECTION: Parsing --
----------------------
type Coord = (Integer, Integer, Integer)

type LavaCoord = Coord

type AirCoord = Coord

type Parser a = Parsec Void String a

parseLavaCoords :: String -> HashSet LavaCoord
parseLavaCoords =
  HashSet.fromList
    . rights
    . map (runParser coordParser "")
    . lines

coordParser :: Parser LavaCoord
coordParser =
  (,,)
    <$> (L.decimal <* char ',')
    <*> (L.decimal <* char ',')
    <*> L.decimal

-------------------
-- SECTION: Main --
-------------------
part1 :: HashSet LavaCoord -> Int
part1 lavaCoords =
  sum
    . map (computeSurfaceArea lavaCoords)
    . HashSet.toList
    $ lavaCoords

part2 :: HashSet LavaCoord -> Int
part2 lavaCoords =
  let openAirCoords = getOpenAirCoords lavaCoords
   in sum
        . map (computeOpenSurfaceArea openAirCoords)
        . HashSet.toList
        $ lavaCoords

computeOpenSurfaceArea :: HashSet AirCoord -> LavaCoord -> Int
computeOpenSurfaceArea openAirCoords =
  length
    . filter (flip HashSet.member openAirCoords)
    . getNeighbors

computeSurfaceArea :: HashSet LavaCoord -> LavaCoord -> Int
computeSurfaceArea coords =
  length
    . filter (not . flip HashSet.member coords)
    . getNeighbors

getNeighbors :: Coord -> [Coord]
getNeighbors (x, y, z) =
  [ (x, y, z + 1),
    (x, y, z - 1),
    (x, y + 1, z),
    (x, y - 1, z),
    (x + 1, y, z),
    (x - 1, y, z)
  ]

getOpenAirCoords :: HashSet LavaCoord -> HashSet AirCoord
getOpenAirCoords lavaCoords =
  let allNeighbors = concatMap getNeighbors lavaCoords
      openAirCoords =
        filter
          ((&&) <$> (flip isAirCoord lavaCoords) <*> (flip isOpenCoord lavaCoords))
          allNeighbors
      openAirCoords' = expandOpenAirCoords lavaCoords openAirCoords
   in openAirCoords'

expandOpenAirCoords ::
  Foldable t =>
  HashSet LavaCoord ->
  t AirCoord ->
  HashSet AirCoord
expandOpenAirCoords lavaCoords openAirCoords =
  foldr addCoord HashSet.empty openAirCoords
  where
    addCoord :: Coord -> HashSet AirCoord -> HashSet AirCoord
    addCoord coord airCoords
      | not $ isAirCoord coord lavaCoords = airCoords
      | HashSet.member coord airCoords = airCoords
      | isNextToLava coord lavaCoords = foldr addCoord airCoords' neighbors
      | any (flip isNextToLava lavaCoords) neighbors = foldr addCoord airCoords' neighbors
      | otherwise = airCoords
      where
        airCoords' = HashSet.insert coord airCoords
        neighbors = getNeighbors coord

isLavaCoord :: Coord -> HashSet LavaCoord -> Bool
isLavaCoord = HashSet.member

isAirCoord :: Coord -> HashSet LavaCoord -> Bool
isAirCoord = not .* HashSet.member

isNextToLava :: Coord -> HashSet LavaCoord -> Bool
isNextToLava coord lavaCoords =
  any (flip isLavaCoord lavaCoords) (getNeighbors coord)

isOpenCoord :: Coord -> HashSet LavaCoord -> Bool
isOpenCoord (x, y, z) lavaCoords =
  (null $ HashSet.filter (\(x', y', z') -> x < x' && y == y' && z == z') lavaCoords)
    || (null $ HashSet.filter (\(x', y', z') -> x > x' && y == y' && z == z') lavaCoords)
    || (null $ HashSet.filter (\(x', y', z') -> x == x' && y < y' && z == z') lavaCoords)
    || (null $ HashSet.filter (\(x', y', z') -> x == x' && y > y' && z == z') lavaCoords)
    || (null $ HashSet.filter (\(x', y', z') -> x == x' && y == y' && z < z') lavaCoords)
    || (null $ HashSet.filter (\(x', y', z') -> x == x' && y == y' && z > z') lavaCoords)
