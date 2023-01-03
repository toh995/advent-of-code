module Day23 where

import Data.Counter
import Data.Foldable
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import Data.Maybe
import GHC.Utils.Misc hiding (count, Direction)
import Safe hiding (maximumMay, minimumMay)
import Safe.Foldable

----------------------
-- SECTION: Main IO --
----------------------
filePath :: String
filePath = "data/Day23.txt"

main :: IO ()
main = do
  board <- parseBoard . lines <$> readFile filePath

  let part1Answer = part1 board
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 board
  putStrLn $ "PART 2: " ++ show part2Answer

----------------------
-- SECTION: Parsing --
----------------------
type Line = String

parseBoard :: [Line] -> Board
parseBoard ls =
  Board hashSet
  where
    is = [0 .. length ls - 1]
    hashSet = HashSet.unions
            . map (setForI ls)
            $ is

setForI :: [Line] -> I -> HashSet ElfCoord
setForI ls i =
  let line = fromMaybe [] $
               atMay ls i
      js = [0 .. length line - 1]
      js' = filter isElfJ js
      isElfJ j = maybeToBool $
                   (== '#') <$> atMay line j
      coords = map ((,) i) js'
   in HashSet.fromList coords

--------------------------
-- SECTION: Parts 1 & 2 --
--------------------------
initialDirections :: [Direction]
initialDirections = cycle [North, South, West, East]

part1 :: Board -> Maybe Int
part1 board = do
  let firstRoundNum = 0
  let (board', _, _, _) = nTimes 10 doRound (board, initialDirections, firstRoundNum, Nothing)
  rectangleArea <- getRectangleArea board'
  let coordCount = getCoordCount board'
  return $ rectangleArea - coordCount

part2 :: Board -> Int
part2 board =
  let firstRoundNum = 0
      initialTuple = (board, initialDirections, firstRoundNum, Nothing)
      (_, _, finalRoundNum, _) = until
                                   (\(currBoard, _, _, prevBoard) -> Just currBoard == prevBoard)
                                   doRound
                                   initialTuple
   in finalRoundNum
            
-------------------------
-- SECTION: Core Logic --
-------------------------
type I = Int
type J = Int
type ElfCoord = (I, J)
type Offset = (I, J)

type RoundNum = Int

data Board = Board {
  getCoords :: HashSet ElfCoord
}
  deriving (Eq, Show)

data Direction =
  North
  | South
  | East
  | West
  | NorthEast
  | NorthWest
  | SouthEast
  | SouthWest
  deriving (Bounded, Enum)

data MoveProposal = MoveProposal {
    startCoord :: ElfCoord
  , endCoord :: ElfCoord
}
  deriving (Eq, Show)

allDirections :: [Direction]
allDirections = enumFrom minBound

doRound ::
  (Board, [Direction], RoundNum, Maybe Board)
  -> (Board, [Direction], RoundNum, Maybe Board)
doRound (board, dirs, roundNum, _) =
  let neighborlyCoords = HashSet.toList . getNeighborlyCoords $ board
      currDirs = take 4 dirs
      nextDirs = drop 1 dirs
      proposals = mapMaybe (makeProposalWithDirs board currDirs) neighborlyCoords
      nextBoard = doProposals board proposals
   in (nextBoard, nextDirs, roundNum+1, Just board)

makeProposalWithDirs :: Board -> [Direction] -> ElfCoord -> Maybe MoveProposal
makeProposalWithDirs board dirs startCoord =
  let proposals = mapMaybe (makeProposal startCoord board) dirs
      proposal = headMay proposals
   in proposal

makeProposal :: ElfCoord -> Board -> Direction -> Maybe MoveProposal
makeProposal startCoord board North
  | isValid   = Just $ MoveProposal {startCoord, endCoord = getNeighbor startCoord North}
  | otherwise = Nothing
  where
    isValid = all (not <$> hasNeighbor startCoord board) [North, NorthEast, NorthWest]
makeProposal startCoord board South
  | isValid   = Just $ MoveProposal {startCoord, endCoord = getNeighbor startCoord South}
  | otherwise = Nothing
  where
    isValid = all (not <$> hasNeighbor startCoord board) [South, SouthEast, SouthWest]
makeProposal startCoord board West
  | isValid   = Just $ MoveProposal {startCoord, endCoord = getNeighbor startCoord West}
  | otherwise = Nothing
  where
    isValid = all (not <$> hasNeighbor startCoord board) [West, NorthWest, SouthWest]
makeProposal startCoord board East
  | isValid   = Just $ MoveProposal {startCoord, endCoord = getNeighbor startCoord East}
  | otherwise = Nothing
  where
    isValid = all (not <$> hasNeighbor startCoord board) [East, NorthEast, SouthEast]
makeProposal _ _ _ = Nothing

doProposals :: Board -> [MoveProposal] -> Board
doProposals board proposals =
  let proposals' = removeDupeEndCoords proposals
      toRemove = map startCoord proposals'
      toInsert = map endCoord proposals'
   in insertCoords toInsert
        . removeCoords toRemove
        $ board

removeDupeEndCoords :: [MoveProposal] -> [MoveProposal]
removeDupeEndCoords ps =
  let endCoords = map endCoord ps
      counter = (count endCoords) :: Counter ElfCoord Integer
      validEndCoords = Map.keys . Map.filter (== 1) $ counter
   in filter
        (flip elem validEndCoords . endCoord)
        ps

insertCoords :: Foldable t => t ElfCoord -> Board -> Board
insertCoords toInsert (Board hs) =
  let toInsert' = HashSet.fromList . toList $ toInsert
      hs' = HashSet.union hs toInsert'
   in Board hs'

removeCoords :: Foldable t => t ElfCoord -> Board -> Board
removeCoords toRemove (Board hs) =
  let toRemove' = HashSet.fromList . toList $ toRemove
      hs' = HashSet.difference hs toRemove'
   in Board hs'

-- Set of coords that are adjacent to at least one other coord
getNeighborlyCoords :: Board -> HashSet ElfCoord
getNeighborlyCoords board@(Board coords) =
  HashSet.filter (flip hasAnyNeighbor board) coords

coordInBoard :: ElfCoord -> Board -> Bool
coordInBoard coord (Board hs) =
  HashSet.member coord hs

hasAnyNeighbor :: ElfCoord -> Board -> Bool
hasAnyNeighbor coord board =
  any (hasNeighbor coord board) allDirections

hasNeighbor :: ElfCoord -> Board -> Direction -> Bool
hasNeighbor coord board direction =
  coordInBoard neighbor board
  where
    neighbor = getNeighbor coord direction
  
getNeighbors :: ElfCoord -> [ElfCoord]
getNeighbors coord =
  map (getNeighbor coord) allDirections

getNeighbor :: ElfCoord -> Direction -> ElfCoord
getNeighbor (i, j) North = (i-1, j)
getNeighbor (i, j) South = (i+1, j)
getNeighbor (i, j) East = (i, j+1)
getNeighbor (i, j) West = (i, j-1)
getNeighbor coord NorthEast = flip getNeighbor North $ getNeighbor coord East
getNeighbor coord NorthWest = flip getNeighbor North $ getNeighbor coord West
getNeighbor coord SouthEast = flip getNeighbor South $ getNeighbor coord East
getNeighbor coord SouthWest = flip getNeighbor South $ getNeighbor coord West

getCoordCount :: Board -> Int
getCoordCount (Board coords) = length coords

getRectangleArea :: Board -> Maybe Int
getRectangleArea board = do
  maxI <- getMaxI board
  minI <- getMinI board
  maxJ <- getMaxJ board
  minJ <- getMinJ board
  return $ (maxI - minI + 1) * (maxJ - minJ + 1)

getMaxI :: Board -> Maybe I
getMaxI (Board coords) =
  maximumMay . HashSet.map fst $ coords

getMinI :: Board -> Maybe I
getMinI (Board coords) =
  minimumMay . HashSet.map fst $ coords

getMaxJ :: Board -> Maybe J
getMaxJ (Board coords) =
  maximumMay . HashSet.map snd $ coords

getMinJ :: Board -> Maybe J
getMinJ (Board coords) =
  minimumMay . HashSet.map snd $ coords

--------------------
-- SECTION: Utils --
--------------------
maybeToBool :: Maybe Bool -> Bool
maybeToBool Nothing = False
maybeToBool (Just b) = b
