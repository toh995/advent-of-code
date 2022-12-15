module Day15 where

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List
import Data.Maybe
import Data.Ranges
import Data.Void
import GHC.Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

----------------------
-- SECTION: Main IO --
----------------------
filePath :: String
filePath = "data/Day15.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath
  let board = parseLines . lines $ inputStr

  let part1Answer = part1 board
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 board
  putStrLn $ "PART 2: " ++ show part2Answer

----------------------
-- SECTION: Parsing --
----------------------
type Line = String

type Parser a = Parsec Void String a

parseLines :: [Line] -> Board
parseLines ls =
  let coordPairs = mapMaybe parseLine ls
      board = boardFromCoordPairs coordPairs
   in board

parseLine :: Line -> Maybe CoordPair
parseLine = rightToMaybe . runParser coordPairParser ""

signedInt :: Parser Int
signedInt = L.signed space L.decimal

coordPairParser :: Parser CoordPair
coordPairParser = (,) <$> sensorCoordParser <*> beaconCoordParser

sensorCoordParser :: Parser SensorCoord
sensorCoordParser =
  (,)
    <$> (string "Sensor at x=" *> signedInt)
    <*> (string ", y=" *> signedInt)

beaconCoordParser :: Parser BeaconCoord
beaconCoordParser =
  (,)
    <$> (string ": closest beacon is at x=" *> signedInt)
    <*> (string ", y=" *> signedInt)

-------------------
-- SECTION: Main --
-------------------
part1 :: Board -> Int
part1 board =
  let y = 2000000
      xRanges = noBeaconXs y board
   in length . fromRanges $ xRanges

part2 :: Board -> [Int]
part2 board =
  let xMin = 0
      xMax = 4000000
      ys = [0..4000000]
      candidateCoords =
        concatMap
          (findBeaconCoords xMin xMax board)
          ys
      beaconCoords = getBeaconCoords board
      finalCoords = candidateCoords \\ beaconCoords
   in map computeTuningFrequency finalCoords

computeTuningFrequency :: Coord -> Int
computeTuningFrequency (x, y) = y + (x * 4000000)

getDistance :: Coord -> Coord -> Int
getDistance (x, y) (x', y') = abs (x - x') + abs (y - y')

getCoords :: Y -> Ranges X -> [Coord]
getCoords y xRanges = (,) <$> fromRanges xRanges <*> [y]

singletonRanges :: a -> Ranges a
singletonRanges x = x +=+ x

-----------------------------------
-- SECTION: Board Data Structure --
-----------------------------------
data Board = Board {
    coordPairs :: [(SensorCoord, BeaconCoord)]
  , beaconYtoXs :: HashMap Y XSet
  , sensorYtoXs :: HashMap Y XSet
}
type X = Int
type Y = Int
type Coord = (X, Y)
type SensorCoord = Coord
type BeaconCoord = Coord
type CoordPair = (SensorCoord, BeaconCoord)
type XSet = IntSet


boardFromCoordPairs :: [CoordPair] -> Board
boardFromCoordPairs coordPairs =
  let beaconCoords = map snd coordPairs
      beaconYtoXs = buildYtoXs beaconCoords
      sensorCoords = map fst coordPairs
      sensorYtoXs = buildYtoXs sensorCoords
   in Board { coordPairs, beaconYtoXs, sensorYtoXs }

buildYtoXs :: [Coord] -> HashMap Y XSet
buildYtoXs =
  foldr f HashMap.empty
  where
    f (x, y) hm =
      let set = HashMap.findWithDefault IntSet.empty y hm
          set' = IntSet.insert x set
          hm' = HashMap.insert y set' hm
       in hm'

getBeaconCoords :: Board -> [BeaconCoord]
getBeaconCoords (Board {coordPairs}) = map snd coordPairs

-- For the given y value, compute the x ranges where
-- there are no beacons on the given y value
noBeaconXs :: Y -> Board -> Ranges X
noBeaconXs y board =
  (xsExcludedBySensors y board <> sensorXs y board)
    `difference`
    (beaconXs y board)

sensorXs :: Y -> Board -> Ranges X
sensorXs y (Board {sensorYtoXs}) =
  let xSet = HashMap.findWithDefault IntSet.empty y sensorYtoXs
      xList = IntSet.toList xSet
      ranges = foldMap singletonRanges xList
   in ranges

beaconXs :: Y -> Board -> Ranges X
beaconXs y (Board {beaconYtoXs}) =
  let xSet = HashMap.findWithDefault IntSet.empty y beaconYtoXs
      xList = IntSet.toList xSet
      ranges = foldMap singletonRanges xList
   in ranges

xsExcludedBySensors :: Y -> Board -> Ranges X
xsExcludedBySensors y (Board {coordPairs}) =
  foldMap (xsExcludedBySensor y) coordPairs

xsExcludedBySensor :: Y -> (SensorCoord, BeaconCoord) -> Ranges X
xsExcludedBySensor y (sensorCoord@(sx, sy), beaconCoord) =
  let maxDist = getDistance sensorCoord beaconCoord
      distToY = abs $ y - sy
      remaining = maxDist - distToY
   in if remaining < 0
      then mempty
      else (sx - remaining) +=+ (sx + remaining)

-- At the given y, find all of the possible beacon coordinates
findBeaconCoords :: X -> X -> Board -> Y -> [BeaconCoord]
findBeaconCoords xMin xMax board y =
  let xRanges = (xMin +=+ xMax) `difference` (noBeaconXs y board)
   in getCoords y xRanges
