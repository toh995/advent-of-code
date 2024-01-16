module Day18.Part02 where

import Data.Char
import Data.Either
import Data.Functor
import Data.List.Split
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Direction = U | D | L | R
  deriving (Eq, Ord, Show)

data Command = Command
  { dir :: Direction
  , magnitude :: Int
  }
  deriving (Eq, Show)

type I = Int
type J = Int
type Coord = (I, J)

-- Main IO
filePath :: String
filePath = "src/Day18/data.txt"

main :: IO ()
main = do
  cmds <- readFile filePath <&> parseCommands
  -- mapM_ print cmds
  -- print (sum . map magnitude $ cmds)
  -- mapM_ print $ take 10 cmds
  -- let borderCoords = getBorderCoords cmds
  -- let iMin = (minimum . map fst $ borderCoords) - 1
  -- let iMax = (maximum . map fst $ borderCoords) + 1
  -- let jMin = (minimum . map snd $ borderCoords) - 1
  -- let jMax = (maximum . map fst $ borderCoords) + 1
  -- mapM_ print [iMin, iMax, jMin, jMax]
  -- print $ length borderCoords
  -- print $ map dir cmds
  print (all ((||) <$> (== R) <*> (== L)) . map dir . concat . divvy 1 2 $ cmds)
  print (all ((||) <$> (== U) <*> (== D)) . map dir . concat . divvy 1 2 . drop 1 $ cmds)
  print (dir . head $ cmds)

-- Core logic
getBorderCoords :: [Command] -> [Coord]
getBorderCoords =
  scanr getNextCoord (0, 0)

getNextCoord :: Command -> Coord -> Coord
getNextCoord
  (Command{dir, magnitude})
  (i, j) =
    case dir of
      U -> (i - magnitude, j)
      D -> (i + magnitude, j)
      L -> (i, j - magnitude)
      R -> (i, j + magnitude)

-- getBorderCoords :: [Command] -> [Coord]
-- getBorderCoords cmds =
--   finalCoords
--  where
--   (_, finalCoords) = foldr f ((0, 0), mempty) cmds
--   f cmd (currCoord, coords) =
--     let nextCoord = getNextCoord cmd currCoord
--         newCoords = getCoordsForCmd cmd currCoord
--      in ( nextCoord
--         , coords ++ newCoords
--         )

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
  _ <- skipMany (anySingleBut '(') >> string "(#"
  magnitude <- count 5 hexDigitChar <&> hexToInt
  dir <- directionP <* char ')'
  pure Command{magnitude, dir}

directionP :: Parser Direction
directionP =
  (char '3' $> U)
    <|> (char '1' $> D)
    <|> (char '2' $> L)
    <|> (char '0' $> R)

hexToInt :: String -> Int
hexToInt chars =
  let digits = digitToInt <$> chars
      powers = (16 ^) <$> ([0 ..] :: [Int])
   in sum $
        zipWith (*) (reverse digits) powers

{-
Algorithm
Partition vert segs into subsets, where
  each subset represents a single vertical line

Sort subsets from left to right

Within each subset...

Sort the vert segs from top to bottom
Each vert seg has a top region and bot region
  - Extend the region, ONLY IF ITS INSIDE
  - Extend until we hit a hori seg
    - If the hori seg coincides with a SECOND vert seg, then
      concatenate and continue

After accumulating the extended vert segs...
  TODO

To tell whether a region is in or out:
  For the FIRST region
    Count how many hori segs intersect
      | even -> OUT
      | odd -> IN
  OTHERWISE
    Check shape
    PRESERVES:
     -
    |
     -
    CHANGES:
    -
     |
      -
-}
