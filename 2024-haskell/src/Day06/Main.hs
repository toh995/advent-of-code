module Day06.Main where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Data.Functor
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable
import GHC.Generics (Generic)

type I = Int
type J = Int
type Coord = (I, J)

data Tile = Empty | Obstacle
  deriving (Eq, Show)

type Matrix = HashMap Coord Tile

data Position = Position
  { coord :: Coord
  , dir :: Direction
  }
  deriving (Eq, Generic, Show)
instance Hashable Position

data Direction = U | D | L | R
  deriving (Eq, Generic, Show)
instance Hashable Direction

filePath :: String
filePath = "src/Day06/data.txt"

main :: IO ()
main =
  runExceptT main' >>= \case
    Left err -> error err
    _ -> pure ()

main' :: (MonadIO m, MonadError String m) => m ()
main' = do
  (matrix, firstPos) <- parse =<< liftIO (readFile filePath)
  let part1Answer = part1 matrix firstPos
  liftIO $ putStrLn ("PART 1: " ++ show part1Answer)
  let part2Answer = part2 matrix firstPos
  liftIO $ putStrLn ("PART 2: " ++ show part2Answer)

part1 :: Matrix -> Position -> Int
part1 matrix startPos =
  length
    . HashSet.fromList
    . map coord
    $ traversePath startPos matrix

part2 :: Matrix -> Position -> Int
part2 matrix startPos =
  length
    . filter isCyclic
    . map (traversePath startPos)
    $ matrices
 where
  matrices :: [Matrix]
  matrices =
    map (\(coord, _) -> HashMap.insert coord Obstacle matrix)
      . filter ((/= startPos.coord) . fst)
      . filter ((== Empty) . snd)
      $ HashMap.toList matrix

isCyclic :: [Position] -> Bool
isCyclic = isCyclic' HashSet.empty
 where
  isCyclic' :: HashSet Position -> [Position] -> Bool
  isCyclic' _ [] = False
  isCyclic' seen (p : ps)
    | HashSet.member p seen = True
    | otherwise = isCyclic' seen' ps
   where
    seen' = HashSet.insert p seen

traversePath :: Position -> Matrix -> [Position]
traversePath startPos matrix =
  case move matrix startPos of
    Just nextPos -> nextPos : traversePath nextPos matrix
    Nothing -> []

move :: Matrix -> Position -> Maybe Position
move matrix p =
  HashMap.lookup (nextCoord p) matrix
    >>= \case
      Empty -> pure p{coord = nextCoord p}
      Obstacle ->
        let p' = p{dir = turnClockwise p.dir}
         in move matrix p'

-- p'{coord = nextCoord p'}

nextCoord :: Position -> Coord
nextCoord Position{coord = (i, j), dir} =
  case dir of
    U -> (i - 1, j)
    D -> (i + 1, j)
    L -> (i, j - 1)
    R -> (i, j + 1)

turnClockwise :: Direction -> Direction
turnClockwise U = R
turnClockwise R = D
turnClockwise D = L
turnClockwise L = U

-------------
-- PARSING --
-------------
parse :: (MonadError String m) => String -> m (Matrix, Position)
parse inputStr =
  execStateT (parseS inputStr) initialS
    <&> \s -> (s.matrix, s.firstPos)

data S = S
  { currCoord :: Coord
  , firstPos :: Position
  , matrix :: Matrix
  }

initialS :: S
initialS =
  S
    { currCoord = (0, 0)
    , firstPos = Position{coord = (0, 0), dir = U}
    , matrix = HashMap.empty
    }

parseS :: (MonadState S m, MonadError String m) => String -> m ()
parseS = mapM_ processChar

processChar :: (MonadState S m, MonadError String m) => Char -> m ()
processChar '\n' = modify (\s@S{currCoord = (i, _)} -> s{currCoord = (i + 1, 0)})
processChar c = do
  tile <- charToTile c
  modify $
    \s@S{currCoord, matrix} ->
      s{matrix = HashMap.insert currCoord tile matrix}
  when
    (c == '^')
    (modify $ \s -> s{firstPos = Position{coord = s.currCoord, dir = U}})
  modify $
    \s@S{currCoord = (i, j)} ->
      s{currCoord = (i, j + 1)}

charToTile :: (MonadError String m) => Char -> m Tile
charToTile '#' = pure Obstacle
charToTile '.' = pure Empty
charToTile '^' = pure Empty
charToTile c = throwError $ "Could not convert char '" ++ show c ++ "' to a tile"
