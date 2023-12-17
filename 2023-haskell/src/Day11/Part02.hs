module Day11.Part02 where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Either
import Data.Functor
import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as IntMap
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type I = Int
type J = Int
type Coord = (I, J)

galaxyChar :: Char
galaxyChar = '#'

emptyChar :: Char
emptyChar = '.'

filePath :: String
filePath = "src/Day11/data.txt"

main :: IO ()
main = do
  rows <- readFile filePath <&> lines

  let part2Answer = part2 rows
  putStrLn $ "PART 2: " ++ show part2Answer

part2 :: [[Char]] -> Maybe Int
part2 rows = do
  iMap <- buildIdxMap rows
  jMap <- buildIdxMap . transpose $ rows
  let galaxyCoords = parseGalaxyCoords rows
  galaxyCoords' <- mapM (translateCoord iMap jMap) galaxyCoords

  let coordPairs = getAllPairs galaxyCoords'
  let answer = sum . map (uncurry manhattanDistance) $ coordPairs
  pure answer

translateCoord :: IntMap Int -> IntMap Int -> Coord -> Maybe Coord
translateCoord iMap jMap (i, j) = do
  i' <- IntMap.lookup i iMap
  j' <- IntMap.lookup j jMap
  pure (i', j')

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (i, j) (i', j') =
  abs (i - i') + abs (j - j')

getAllPairs :: (Ord a) => [a] -> [(a, a)]
getAllPairs as = [(x, y) | x <- as, y <- as, x < y]

-- Building the map
buildIdxMap :: [[Char]] -> Maybe (IntMap Int)
buildIdxMap rows =
  execStateT
    (mapM_ fooS $ zip [0 .. length rows - 1] rows)
    (IntMap.fromList [(-1, 0)])

fooS :: (Int, [Char]) -> StateT (IntMap Int) Maybe ()
fooS (i, row) = do
  im <- get
  prevV <- lift $ IntMap.lookup (i - 1) im
  let currV =
        if all (== emptyChar) row
          then prevV + 1_000_000
          else prevV + 1
  put $ IntMap.insert i currV im

-- Getting coordinates
type Parser = Parsec Void String

parseGalaxyCoords :: [String] -> [Coord]
parseGalaxyCoords =
  fromRight []
    . runParser galaxyCoordsP ""
    . unlines

galaxyCoordsP :: Parser [Coord]
galaxyCoordsP =
  delimP
    *> galaxyCoordP `sepEndBy` delimP

delimP :: Parser ()
delimP =
  skipMany $
    char emptyChar
      <|> newline

galaxyCoordP :: Parser Coord
galaxyCoordP = do
  _ <- char galaxyChar
  pos <- getSourcePos
  let i = subtract 1 . unPos . sourceLine $ pos
  let j = subtract 2 . unPos . sourceColumn $ pos
  pure (i, j)
