module Day18.Part01 where

import Control.Monad.Trans.State.Lazy
import Data.Either
import Data.Functor
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- Coord/Box Primitives
type I = Int
type J = Int
type Coord = (I, J)

data Box = Box
  { iMin, iMax :: I
  , jMin, jMax :: J
  }

getAllCoords :: Box -> [Coord]
getAllCoords b =
  (,) <$> [iMin b .. iMax b] <*> [jMin b .. jMax b]

isValidCoord :: Box -> Coord -> Bool
isValidCoord b (i, j) =
  (iMin b <= i && i <= iMax b)
    && (jMin b <= j && j <= jMax b)

getNeighbors :: Box -> Coord -> [Coord]
getNeighbors b (i, j) =
  filter
    (isValidCoord b)
    [ (i - 1, j)
    , (i + 1, j)
    , (i, j - 1)
    , (i, j + 1)
    ]

data Direction = U | D | L | R
  deriving (Eq, Ord, Show)

data Command = Command
  { dir :: Direction
  , magnitude :: Int
  }
  deriving (Show)

-- Main IO
filePath :: String
filePath = "src/Day18/data.txt"

main :: IO ()
main = do
  commands <- readFile filePath <&> parseCommands

  let part1Answer = part1 commands
  putStrLn $ "PART 1: " ++ show part1Answer

part1 :: [Command] -> Int
part1 cmds =
  let borderCoords = getBorderCoords cmds
      box =
        Box
          { iMin = (minimum . map fst $ borderCoords) - 1
          , iMax = (maximum . map fst $ borderCoords) + 1
          , jMin = (minimum . map snd $ borderCoords) - 1
          , jMax = (maximum . map fst $ borderCoords) + 1
          }
      allCoords = getAllCoords box
      outerCoords = getOuterCoords box borderCoords
   in length allCoords - length outerCoords

getOuterCoords :: Box -> [Coord] -> [Coord]
getOuterCoords box borderCoords =
  HashSet.toList $
    execState
      (f startCoord)
      mempty
 where
  startCoord = (iMin box, jMin box)
  borderSet = HashSet.fromList borderCoords
  f :: Coord -> State (HashSet Coord) ()
  f coord = do
    seen <- get
    put $ HashSet.insert coord seen
    let neighbors =
          filter (not . (`HashSet.member` borderSet))
            . filter (not . (`HashSet.member` seen))
            . getNeighbors box
            $ coord
    mapM_ f neighbors

getBorderCoords :: [Command] -> [Coord]
getBorderCoords cmds =
  finalCoords
 where
  (_, finalCoords) = foldr f ((0, 0), mempty) cmds
  f cmd (currCoord, coords) =
    let nextCoord = getNextCoord cmd currCoord
        newCoords = getCoordsForCmd cmd currCoord
     in ( nextCoord
        , coords ++ newCoords
        )

getNextCoord :: Command -> Coord -> Coord
getNextCoord
  (Command{dir, magnitude})
  (i, j) =
    case dir of
      U -> (i - magnitude, j)
      D -> (i + magnitude, j)
      L -> (i, j - magnitude)
      R -> (i, j + magnitude)

getCoordsForCmd :: Command -> Coord -> [Coord]
getCoordsForCmd
  (Command{dir, magnitude})
  (i, j) =
    case dir of
      U -> (,j) <$> [i - magnitude .. i - 1]
      D -> (,j) <$> [i + 1 .. i + magnitude]
      L -> (i,) <$> [j - magnitude .. j - 1]
      R -> (i,) <$> [j + 1 .. j + magnitude]

-- Parsing logic
type Parser = Parsec Void String

parseCommands :: String -> [Command]
parseCommands =
  fromRight []
    . runParser commandsP ""

commandsP :: Parser [Command]
commandsP = commandP `sepEndBy` newline

commandP :: Parser Command
commandP = do
  dir <- directionP <* hspace
  magnitude <- L.decimal <* skipMany (anySingleBut '\n')
  pure Command{dir, magnitude}

directionP :: Parser Direction
directionP =
  (char 'U' $> U)
    <|> (char 'D' $> D)
    <|> (char 'L' $> L)
    <|> (char 'R' $> R)
