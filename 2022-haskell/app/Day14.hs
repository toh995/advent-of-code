module Day14 where

import Control.Applicative
import Data.Composition
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List
import Data.List.Split
import Data.Maybe
import GHC.Utils.Misc
import Text.Read

type Line = String

data Board = Board {
  hashMap :: HashMap Int Column,
  yFloor :: Maybe Y
}
  deriving (Eq, Show)

type Column = IntSet
type X = Int
type Y = Int
type Counter = Integer
type Coord = (X, Y)

----------------------
-- SECTION: Main IO --
----------------------
filePath :: String
filePath = "data/Day14.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath
  let board = strToBoard inputStr
  
  let part1Answer = part1 board
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 board
  putStrLn $ "PART 2: " ++ show part2Answer

----------------------
-- SECTION: Parsing --
----------------------
strToBoard :: String -> Board
strToBoard = coordsToBoard . linesToCoords . lines

coordsToBoard :: [Coord] -> Board
coordsToBoard = foldr f emptyBoard
  where
    f (x, y) board = insertCoord board x y

linesToCoords :: [Line] -> [Coord]
linesToCoords = concatMap lineToCoords

lineToCoords :: Line -> [Coord]
lineToCoords l =
  let endpoints = mapMaybe strToCoord . splitOn " -> " $ l
      coords = nub
             . concat
             . mapMaybe (uncurry buildCoordList)
             . mapMaybe listToPair
             . divvy 2 1
             $ endpoints
   in coords

-- strToIntPair "12,5" == Just (12, 5)
strToCoord :: String -> Maybe Coord
strToCoord s =
  let stringPair = listToPair . splitOn "," $ s
   in stringPair >>= firstM readMaybe >>= secondM readMaybe

-- Given two "endpoints", get all coords within the two endpoints
buildCoordList :: Coord -> Coord -> Maybe [Coord]
buildCoordList (x, y) (x', y')
  | x /= x' && y /= y' = Nothing
  | otherwise          = Just $ (,)
                           <$> [(min x x')..(max x x')]
                           <*> [(min y y')..(max y y')]

-- fixme: extract into util??
listToPair :: [a] -> Maybe (a, a)
listToPair xs
  | length xs /= 2 = Nothing
  | otherwise      = Just (xs !! 0, xs !! 1)

------------------------
-- SECTION: Main Algo --
------------------------
startX :: X
startX = 500

startY :: Y
startY = 0

part1 :: Board -> Counter
part1 board =
  let board' = board {yFloor = Nothing}
      counter = 0
   in countNewUnits board' startX startY counter

part2 :: Board -> Counter
part2 board =
  let board' = board {yFloor = Just $ 2 + getYmax board}
      counter = 0
   in countNewUnits board' startX startY counter

countNewUnits :: Board -> X -> Y -> Counter -> Counter
countNewUnits board x y counter
  | coordExists board x y = counter
  | coordExists board x (y+1) = if
    | not $ coordExists board (x-1) (y+1) -> countNewUnits board (x-1) (y+1) counter
    | not $ coordExists board (x+1) (y+1) -> countNewUnits board (x+1) (y+1) counter
    | otherwise -> countNewUnits (insertCoord board x y) startX startY (counter+1)
  | otherwise =
      case (findNextY board x y) of
        (Nothing) -> counter
        (Just y') -> countNewUnits board x (y'-1) counter

--------------------------
-- SECTION: Board Utils --
--------------------------
emptyBoard :: Board
emptyBoard = Board {
  hashMap = HashMap.empty,
  yFloor = Nothing
}

emptyColumn :: Column
emptyColumn = IntSet.empty

insertCoord :: Board -> X -> Y -> Board
insertCoord board x y =
  let column = findColumnWithDefault emptyColumn x board
      column' = insertIntoColumn y column
      board' = insertColumnToBoard x column' board
   in board'

coordExists :: Board -> X -> Y -> Bool
coordExists board x y =
  let column = findColumn x board
      cond1 = fromMaybe False $ yExistsInColumn y <$> column
      cond2 = fromMaybe False $ (y >=) <$> yFloor board
   in cond1 || cond2

findNextY :: Board -> X -> Y -> Maybe Y
findNextY board x y =
  let y' = findColumn x board >>= IntSet.lookupGE y
      y'' = (min <$> y' <*> yFloor board)
              <|> y'
              <|> yFloor board
   in y''

findColumn :: X -> Board -> Maybe Column
findColumn x  = HashMap.lookup x . hashMap

findColumnWithDefault :: Column -> X -> Board -> Column
findColumnWithDefault c = fromMaybe c .* findColumn

insertColumnToBoard :: X -> Column -> Board -> Board
insertColumnToBoard x c board@(Board {hashMap}) =
  board { hashMap = HashMap.insert x c hashMap }

insertIntoColumn :: Y -> Column -> Column
insertIntoColumn = IntSet.insert

yExistsInColumn :: Y -> Column -> Bool
yExistsInColumn = IntSet.member

getColumns :: Board -> [Column]
getColumns = HashMap.elems . hashMap

getYs :: Board -> IntSet
getYs = IntSet.unions . getColumns

getYmax :: Board -> Y
getYmax = IntSet.findMax . getYs
