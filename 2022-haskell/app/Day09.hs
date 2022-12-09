module Day09 where

import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Text.Read
import Prelude hiding (Left, Right)

type Line = String

data Direction = Up | Down | Left | Right
  deriving (Eq, Show)

type Coord = (Int, Int)

type LeadCoord = Coord

type FollowCoord = Coord

type TailCoord = Coord

type Position = [Coord]

instance Num a => Num (a, a) where
  (+) (x, y) (x', y') = (x + x', y + y')
  (*) (x, y) (x', y') = (x * x', y * y')
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger n = (fromInteger n, fromInteger n)
  negate (x, y) = (negate x, negate y)

filePath :: String
filePath = "data/Day09.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath
  let directions = parseLines . lines $ inputStr

  let part1Answer = part1 directions
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 directions
  putStrLn $ "PART 2: " ++ show part2Answer

parseLines :: [Line] -> [Direction]
parseLines = concat . mapMaybe parseLine

parseLine :: Line -> Maybe [Direction]
parseLine ('U' : ' ' : strNum) = replicate <$> readMaybe strNum <*> pure Up
parseLine ('D' : ' ' : strNum) = replicate <$> readMaybe strNum <*> pure Down
parseLine ('L' : ' ' : strNum) = replicate <$> readMaybe strNum <*> pure Left
parseLine ('R' : ' ' : strNum) = replicate <$> readMaybe strNum <*> pure Right
parseLine _ = Nothing

part1 :: [Direction] -> Int
part1 directions =
  let positions = executeDirections 2 directions
      tailCoords = getTails positions
      numTails = Set.size . Set.fromList $ tailCoords
   in numTails

part2 :: [Direction] -> Int
part2 directions =
  let positions = executeDirections 10 directions
      tailCoords = getTails positions
      numTails = Set.size . Set.fromList $ tailCoords
   in numTails

getTails :: [Position] -> [TailCoord]
getTails = mapMaybe (listToMaybe . reverse)

executeDirections :: Int -> [Direction] -> [Position]
executeDirections numKnots directions =
  let initialPosition = replicate numKnots (0, 0)
   in scanl' executeDirection initialPosition directions

-- Compute a new position, according to the given direction
executeDirection :: Position -> Direction -> Position
executeDirection [] _ = []
executeDirection (headCoord : others) direction =
  let newHead = move direction headCoord
   in scanl' follow newHead others

move :: Direction -> Coord -> Coord
move Up = (+) (0, 1)
move Down = (+) (0, -1)
move Left = (+) (-1, 0)
move Right = (+) (1, 0)

-- Given a leader and follower coord,
-- Return the new position for the follower
follow :: LeadCoord -> FollowCoord -> FollowCoord
follow (lx, ly) (fx, fy)
  | abs (lx - fx) <= 1 && abs (ly - fy) <= 1 = (fx, fy)
  | otherwise = a . b . c . d $ (fx, fy)
  where
    a f@(fx', _)
      | (lx - fx') > 0 = move Right f
      | otherwise = f
    b f@(fx', _)
      | (lx - fx') < 0 = move Left f
      | otherwise = f
    c f@(_, fy')
      | (ly - fy') > 0 = move Up f
      | otherwise = f
    d f@(_, fy')
      | (ly - fy') < 0 = move Down f
      | otherwise = f
