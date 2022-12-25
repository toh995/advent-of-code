module Day17 where

import Data.Data
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Utils.Misc hiding (Direction)
import Prelude hiding (Left, Right)

----------------------
-- SECTION: Main IO --
----------------------
filePath :: String
filePath = "data/Day17.txt"

main :: IO ()
main = do
  directions <- charsToDirections <$> readFile filePath

  let part1Answer = part1 directions
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 directions
  putStrLn $ "PART 2: " ++ show part2Answer

----------------------
-- SECTION: Parsing --
----------------------
charsToDirections :: [Char] -> [Direction]
charsToDirections = mapMaybe charToDirection

charToDirection :: Char -> Maybe Direction
charToDirection '>' = Just Right
charToDirection '<' = Just Left
charToDirection _   = Nothing

-------------------------
-- SECTION: Core Logic --
-------------------------
type X = Int
type Y = Int
type YSet = IntSet
type YHeight = Y
type YMax = Y

data Coord = Coord {
    getX :: X
  , getY :: Y
}
  deriving (Data, Eq, Show, Typeable)

type RoundNumber = Int
type DirectionIndex = Int
type ShapeIndex = Int
type YHeightMap = HashMap X YHeight

type StateMap =
  HashMap DirectionIndex
    (HashMap ShapeIndex
      (YHeightMap, RoundNumber, YMax))

data Board = Board {
    xLeftWall :: X
  , xRightWall :: X
  , yFloor :: Y
  , boardCoords :: HashMap X (YSet)
}
  deriving (Show)

data Direction = Right | Left | Down
  deriving (Eq, Show)

data Shape =
  HorizontalLine { shapeCoords :: [Coord]}
  | Plus { shapeCoords :: [Coord]}
  | L { shapeCoords :: [Coord]}
  | VerticalLine { shapeCoords :: [Coord]}
  | Square { shapeCoords :: [Coord]}
  deriving (Data, Show, Typeable)

numberOfShapes :: Int
numberOfShapes = length
               . dataTypeConstrs
               . dataTypeOf
               $ (undefined :: Shape)

emptyBoard :: X -> X -> Y -> Board
emptyBoard xLeftWall xRightWall yFloor =
  Board xLeftWall xRightWall yFloor HashMap.empty

moveCoords :: (Coord -> Coord) -> Shape -> Shape
moveCoords f shape =
  let currCoords = shapeCoords shape
      newCoords = map f currCoords
   in shape { shapeCoords = newCoords }

move :: Direction -> Shape -> Shape
move Right = moveCoords $ (\(Coord x y) -> Coord (x+1) y)
move Left  = moveCoords $ (\(Coord x y) -> Coord (x-1) y)
move Down  = moveCoords $ (\(Coord x y) -> Coord x (y-1))

shapeIsValid :: Board -> Shape -> Bool
shapeIsValid board shape =
  let coords = shapeCoords shape
   in all (isValidCoord board) coords

isValidCoord :: Board -> Coord -> Bool
isValidCoord board coord@(Coord x y) =
  isValidX board x
    && isValidY board y
    && not (isCoordInBoard board coord)

isValidX :: Board -> X -> Bool
isValidX (Board {xLeftWall, xRightWall}) x =
  xLeftWall < x && x < xRightWall

isValidY :: Board -> Y -> Bool
isValidY (Board {yFloor}) y =
  y > yFloor

isCoordInBoard :: Board -> Coord -> Bool
isCoordInBoard (Board {boardCoords}) (Coord x y) =
  let ys = HashMap.findWithDefault IntSet.empty x boardCoords
   in IntSet.member y ys

getMaxY :: Board -> Y
getMaxY (Board {boardCoords, yFloor})
  | IntSet.null ys = yFloor
  | otherwise      = IntSet.findMax ys
  where
    ys = IntSet.unions . HashMap.elems $ boardCoords

getYHeightMap :: Board -> YHeightMap
getYHeightMap board@(Board{boardCoords}) =
  let globalMax = getMaxY board
   in HashMap.map ((+ globalMax) . negate . IntSet.findMax) boardCoords

insertIntoBoard :: Shape -> Board -> Board
insertIntoBoard shape board =
   foldr
     insertCoordIntoBoard
     board
     (shapeCoords shape)

insertCoordIntoBoard :: Coord -> Board -> Board
insertCoordIntoBoard (Coord x y) board@(Board {boardCoords}) =
  let ys = HashMap.findWithDefault IntSet.empty x boardCoords
      ys' = IntSet.insert y ys
      boardCoords' = HashMap.insert x ys' boardCoords
   in board { boardCoords = boardCoords' }

getNewShape :: RoundNumber -> Board -> Shape
getNewShape roundNum board@(Board {xLeftWall})
  | roundNum `mod` numberOfShapes == 0 =
      HorizontalLine [
          Coord (xLeftWall+3) botY
        , Coord (xLeftWall+4) botY
        , Coord (xLeftWall+5) botY
        , Coord (xLeftWall+6) botY
      ]
  | roundNum `mod` numberOfShapes == 1 =
      Plus [
          Coord (xLeftWall+3) (botY+1)
        , Coord (xLeftWall+4) (botY+2)
        , Coord (xLeftWall+4) (botY+1)
        , Coord (xLeftWall+4) botY
        , Coord (xLeftWall+5) (botY+1)
      ]
  | roundNum `mod` numberOfShapes == 2 =
      L [
          Coord (xLeftWall+3) botY
        , Coord (xLeftWall+4) botY
        , Coord (xLeftWall+5) botY
        , Coord (xLeftWall+5) (botY+1)
        , Coord (xLeftWall+5) (botY+2)
      ]
  | roundNum `mod` numberOfShapes == 3 =
      VerticalLine [
          Coord (xLeftWall+3) botY
        , Coord (xLeftWall+3) (botY+1)
        , Coord (xLeftWall+3) (botY+2)
        , Coord (xLeftWall+3) (botY+3)
      ]
  | roundNum `mod` numberOfShapes == 4 =
      Square [
          Coord (xLeftWall+3) botY
        , Coord (xLeftWall+3) (botY+1)
        , Coord (xLeftWall+4) botY
        , Coord (xLeftWall+4) (botY+1)
      ]
  | otherwise = getNewShape (negate roundNum) board
  where
    botY = 4 + getMaxY board

getDirection :: Vector Direction -> DirectionIndex -> Direction
getDirection ds dIdx = (Vector.unsafeIndex) ds idx
  where idx = dIdx `mod` Vector.length ds

findNextShape ::
  Board
  -> Vector Direction
  -> DirectionIndex
  -> Shape
  -> (Shape, DirectionIndex)
findNextShape board ds dIdx shape
  | not $ shapeIsValid board nextShape = if
    | d == Left -> findNextShape board ds (dIdx+1) shape
    | d == Right -> findNextShape board ds (dIdx+1) shape
    | otherwise -> (shape, dIdx+1)
  | otherwise = findNextShape board ds (dIdx+1) nextShape
  where
    d = getDirection ds dIdx
    nextShape = move d shape

doRound ::
  (Board, Vector Direction, DirectionIndex, RoundNumber, StateMap)
  -> (Board, Vector Direction, DirectionIndex , RoundNumber, StateMap)
doRound (board, ds, dIdx, roundNum, stateMap) =
  let stateMap' =
        insertState
          board
          (dIdx `mod` Vector.length ds)
          roundNum
          stateMap
      newShape = getNewShape roundNum board
      (nextShape, dIdx') = findNextShape board ds dIdx newShape
      board' = insertIntoBoard nextShape board
      roundNum' = roundNum + 1
   in (board', ds, dIdx', roundNum', stateMap')

doRounds :: Int -> [Direction] -> Y
doRounds totalNumRounds directions =
  let directions' = intersperse Down directions ++ [Down]
      directions'' = Vector.fromList directions'
      dIdx = 0
      xLeftWall = 0
      xRightWall = 8
      yFloor = 0
      board = emptyBoard xLeftWall xRightWall yFloor
      startRoundNum = 0
      (board', ds', dIdx', roundNum', stateMap') =
        until
          (\(board'', ds'', dIdx'', roundNum'', stateMap'') ->
            isSeen (dIdx'' `mod` Vector.length ds'') (roundNum'' `mod` numberOfShapes) (getYHeightMap board'') stateMap'')
          doRound
          (board, directions'', dIdx, startRoundNum, emptyStateMap)
      previousRoundNumber = fromMaybe 0 $ getPreviousRoundNumber (dIdx' `mod` Vector.length ds') (roundNum' `mod` numberOfShapes) stateMap'
      cycleLength = roundNum' - previousRoundNumber
      offset = previousRoundNumber
      (numCycles, remainder) = (totalNumRounds - offset) `divMod` cycleLength
      previousYMax = fromMaybe 0 $ getPreviousYMax (dIdx' `mod` Vector.length ds') (roundNum' `mod` numberOfShapes) stateMap'
      currYMax = getMaxY board'
      yPerCycle = currYMax - previousYMax
      (finalBoard, _, _, _, _) = nTimes remainder doRound (board', ds', dIdx', roundNum', stateMap')
      remainderY = getMaxY finalBoard - currYMax
   in ((numCycles-1) * yPerCycle) + currYMax + remainderY

part1 :: [Direction] -> Y
part1 = doRounds 2022

part2 :: [Direction] -> Y
part2 = doRounds 1000000000000

emptyStateMap :: StateMap
emptyStateMap = HashMap.empty

isSeen ::
  DirectionIndex
  -> ShapeIndex
  -> HashMap X YHeight
  -> StateMap
  -> Bool
isSeen dIdx shapeIdx yHeightMap stateMap =
  let currYHeightMap = fstOf3
                         <$> (HashMap.lookup shapeIdx
                                . HashMap.findWithDefault HashMap.empty dIdx
                                $ stateMap)
   in currYHeightMap == Just yHeightMap

insertState ::
  Board
  -> DirectionIndex
  -> RoundNumber
  -> StateMap
  -> StateMap
insertState board dIdx roundNum stateMap =
  let shapeIdx = roundNum `mod` numberOfShapes
      yHeightMap = getYHeightMap board
      maxY = getMaxY board
      innerMap = HashMap.findWithDefault HashMap.empty dIdx stateMap
      innerMap' = HashMap.insert shapeIdx (yHeightMap, roundNum, maxY) innerMap
      stateMap' = HashMap.insert dIdx innerMap' stateMap
   in stateMap'

getPreviousYMax :: DirectionIndex -> ShapeIndex -> StateMap -> Maybe YMax
getPreviousYMax dIdx shapeIdx stateMap =
  let maybeTuple = HashMap.lookup shapeIdx
                 . HashMap.findWithDefault HashMap.empty dIdx
                 $ stateMap
   in thdOf3 <$> maybeTuple

getPreviousRoundNumber :: DirectionIndex -> ShapeIndex -> StateMap -> Maybe RoundNumber
getPreviousRoundNumber dIdx shapeIdx stateMap =
  let maybeTuple = HashMap.lookup shapeIdx
                 . HashMap.findWithDefault HashMap.empty dIdx
                 $ stateMap
   in sndOf3 <$> maybeTuple
