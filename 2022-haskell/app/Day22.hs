module Day22 where

import Data.Either
import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.List.Split
import Data.Maybe
import Data.Void
import Safe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

----------------------
-- SECTION: Main IO --
----------------------
filePath :: String
filePath = "data/Day22.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath

  let boardString = fromMaybe "" $ getBoardString inputStr
  let movesString = fromMaybe "" $ getMovesString inputStr

  let board = parseBoard boardString
  let moves = parseMoves movesString

  let part1Answer = part1 board moves
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 board moves
  putStrLn $ "PART 2: " ++ show part2Answer

----------------------
-- SECTION: Parsing --
----------------------
type Parser a = Parsec Void String a

type ParserError = ParseErrorBundle String Void

type Line = String

type MovesString = String

type BoardString = String

getBoardString :: BoardString -> Maybe BoardString
getBoardString = flip atMay 0 . splitOn "\n\n"

getMovesString :: String -> Maybe MovesString
getMovesString = flip atMay 1 . splitOn "\n\n"

parseMoves :: MovesString -> [Move]
parseMoves =
  fromRight []
    . runParser (many moveParser) ""

moveParser :: Parser Move
moveParser =
  (pure RotateLeft <* char 'L')
    <|> (pure RotateRight <* char 'R')
    <|> (Forward <$> L.decimal)

parseBoard :: BoardString -> Board
parseBoard s =
  let tiles = map (mapMaybe charToTile) . lines $ s
   in tilesToBoard tiles

charToTile :: Char -> Maybe Tile
charToTile '.' = Just Open
charToTile '#' = Just Wall
charToTile ' ' = Just Empty
charToTile _ = Nothing

tilesToBoard :: [[Tile]] -> Board
tilesToBoard rows =
  Board $
    foldr
      insertRow
      HashMap.empty
      (zip rows [1 ..])
  where
    insertRow (row, i) hm =
      foldr
        (\(tile, j) hm' -> HashMap.insert (i, j) tile hm')
        hm
        (zip row [1 ..])

----------------------------
-- SECTION: Parts 1 and 2 --
----------------------------
part1 :: Board -> [Move] -> Integer
part1 board moves =
  let startPos = getStartPosition board
      finalPos = doMoves moves board Part1 startPos
      password = computePassword finalPos
   in password

part2 :: Board -> [Move] -> Integer
part2 board moves =
  let startPos = getStartPosition board
      finalPos = doMoves moves board Part2 startPos
      password = computePassword finalPos
   in password

-------------------------
-- SECTION: Core Logic --
-------------------------
data Part = Part1 | Part2
  deriving (Show)

data Board = Board (HashMap Coord Tile)
  deriving (Show)

data Tile = Open | Wall | Empty
  deriving (Eq, Show)

data Position = Position Coord Facing
  deriving (Show)

type RowIndex = Integer

type ColumnIndex = Integer

type Coord = (RowIndex, ColumnIndex)

data Facing = L | R | U | D
  deriving (Eq, Show)

data Move
  = RotateLeft
  | RotateRight
  | Forward Integer
  deriving (Show)

opposite :: Facing -> Facing
opposite L = R
opposite R = L
opposite U = D
opposite D = U

faceIntValue :: Facing -> Integer
faceIntValue R = 0
faceIntValue D = 1
faceIntValue L = 2
faceIntValue U = 3

emptyBoard :: Board
emptyBoard = Board HashMap.empty

getTile :: Board -> Position -> Tile
getTile (Board hm) (Position coord _) =
  HashMap.findWithDefault Empty coord hm

getStartPosition :: Board -> Position
getStartPosition (Board hm) =
  let coord =
        fromMaybe (1, 1)
          . minimumMay
          . HashMap.keys
          . HashMap.filter (== Open)
          $ hm
      facing = R
   in Position coord facing

computePassword :: Position -> Integer
computePassword (Position (i, j) facing) =
  (1000 * i) + (4 * j) + faceIntValue facing

doMoves :: Foldable t =>
  t Move
  -> Board
  -> Part
  -> Position
  -> Position
doMoves moves board part startPosition =
  foldl' (doMove board part) startPosition moves

doMove :: Board -> Part -> Position -> Move -> Position
-- rotate left
doMove _ _ (Position coord L) RotateLeft = Position coord D
doMove _ _ (Position coord D) RotateLeft = Position coord R
doMove _ _ (Position coord R) RotateLeft = Position coord U
doMove _ _ (Position coord U) RotateLeft = Position coord L
-- rotate right
doMove _ _ (Position coord L) RotateRight = Position coord U
doMove _ _ (Position coord D) RotateRight = Position coord L
doMove _ _ (Position coord R) RotateRight = Position coord D
doMove _ _ (Position coord U) RotateRight = Position coord R
-- move forward
doMove _ _ pos (Forward 0) = pos
doMove board part pos (Forward n) =
  let pos' = moveForward1InBoard board part pos
   in doMove board part pos' (Forward (n - 1))

moveForward1InBoard :: Board -> Part -> Position -> Position
-- move logic for part 1
moveForward1InBoard board Part1 startPos =
  case forwardTile of
    Open -> forwardPos
    Wall -> startPos
    Empty ->
      if backwardTile == Wall
        then startPos
        else backwardPos
  where
    forwardPos = moveForward1 startPos
    forwardTile = getTile board forwardPos
    backwardPos =
      moveForward1 $
        until
          (\pos -> getTile board pos == Empty)
          moveBackward1
          startPos
    backwardTile = getTile board backwardPos
-- move logic for part 2
moveForward1InBoard board Part2 startPos =
  case nextTile of
    Wall -> startPos
    _ -> nextPos
  where
    forwardPos = moveForward1 startPos
    (Position (i, j) _) = forwardPos
    forwardTile = getTile board forwardPos
    (Position _ currFacing) = startPos
    nextTile = getTile board nextPos
    nextPos = if
      | forwardTile /= Empty -> forwardPos
      | otherwise -> if
        | currFacing == U && i == 0 && 51 <= j && j <= 100 -> Position ((j-50) + 150, 1) R
        | currFacing == L && 151 <= i && i <= 200 && j == 0 -> Position (1, (i-150) + 50) D
        | currFacing == U && i == 0 && 101 <= j && j <= 150 -> Position (200, j-100) U
        | currFacing == D && i == 201 && 1 <= j && j <= 50 -> Position (1, 100 + j) D
        | currFacing == R && 151 <= i && i <= 200 && j == 51 -> Position (150, (i-150) + 50) U
        | currFacing == D && i == 151 && 51 <= j && j <= 100 -> Position ((j-50) + 150, 50) L
        | currFacing == L && 101 <= i && i <= 150 && j == 0 -> Position (51 - (i-100), 51) R
        | currFacing == L && 1 <= i && i <= 50 && j == 50 -> Position (151 - i, 1) R
        | currFacing == U && i == 100 && 1 <= j && j <= 50 -> Position (j + 50, 51) R
        | currFacing == L && 51 <= i && i <= 100 && j == 50 -> Position (101, i-50) D
        | currFacing == R && 1 <= i && i <= 50 && j == 151 -> Position (151 - i , 100) L
        | currFacing == R && 101 <= i && i <= 150 && j == 101 -> Position (51 - (i-100), 150) L
        | currFacing == D && i == 51 && 101 <= j && j <= 150 -> Position ((j-100) + 50, 100) L
        | currFacing == R && 51 <= i && i <= 100 && j == 101 -> Position (50, (i-50) + 100) U
        | otherwise -> startPos

moveForward1 :: Position -> Position
moveForward1 (Position (i, j) L) = Position (i, j - 1) L
moveForward1 (Position (i, j) D) = Position (i + 1, j) D
moveForward1 (Position (i, j) R) = Position (i, j + 1) R
moveForward1 (Position (i, j) U) = Position (i - 1, j) U

moveBackward1 :: Position -> Position
moveBackward1 (Position (i, j) L) = Position (i, j + 1) L
moveBackward1 (Position (i, j) D) = Position (i - 1, j) D
moveBackward1 (Position (i, j) R) = Position (i, j - 1) R
moveBackward1 (Position (i, j) U) = Position (i + 1, j) U
