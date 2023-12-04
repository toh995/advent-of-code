module Day03.Main where

import Data.Char
import Data.Either
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.List
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- Matrix Types and Operators
type I = Int
type J = Int
type Coord = (I, J)

data Matrix a = Matrix
  { iMax, jMax :: Int
  , hm :: HashMap Coord a
  }
  deriving (Show)

buildMatrix :: [[a]] -> Matrix a
buildMatrix [] = error "buildMatrix: empty list."
buildMatrix rows@(r : _) =
  Matrix
    { iMax = length rows
    , jMax = length r
    , hm =
        foldr
          insertRow
          HashMap.empty
          (zip rows [0 ..])
    }
 where
  insertRow (row, i) hm =
    foldr
      (\(x, j) hm' -> HashMap.insert (i, j) x hm')
      hm
      (zip row [0 ..])

lookupM :: Coord -> Matrix a -> Maybe a
lookupM coord (Matrix{hm}) = HashMap.lookup coord hm

neighborCoords :: Coord -> [Coord]
neighborCoords (i, j) = do
  i' <- [i, i + 1, i - 1]
  j' <- [j, j + 1, j - 1]
  if (i', j') == (i, j)
    then empty
    else return (i', j')

getNeighborVals :: Matrix a -> Coord -> [a]
getNeighborVals m =
  mapMaybe (flip lookupM m)
    . neighborCoords

-- Parsers
data IntMeta = IntMeta
  { intVal :: Int
  , coords :: [Coord]
  }
  deriving (Show)

data GearMeta = GearMeta Coord
  deriving (Show)

type Parser = Parsec Void String

intMetasP :: Parser [IntMeta]
intMetasP =
  spaceConsumer
    >> sepEndBy intMetaP spaceConsumer
 where
  spaceConsumer = skipMany $ satisfy (not . isDigit)

intMetaP :: Parser IntMeta
intMetaP = do
  ogPos <- getSourcePos
  intVal <- L.decimal
  lastPos <- getSourcePos
  return
    IntMeta
      { intVal
      , coords = posToCoords ogPos lastPos
      }

gearMetasP :: Parser [GearMeta]
gearMetasP =
  spaceConsumer
    >> sepEndBy gearMetaP spaceConsumer
 where
  spaceConsumer = skipMany $ satisfy (/= '*')

gearMetaP :: Parser GearMeta
gearMetaP = do
  _ <- char '*'
  pos <- getSourcePos
  let i = (subtract 1) . unPos . sourceLine $ pos
  let j = (subtract 2) . unPos . sourceColumn $ pos
  return $ GearMeta (i, j)

posToCoords :: SourcePos -> SourcePos -> [Coord]
posToCoords og final =
  let iOg = (subtract 1) . unPos . sourceLine $ og
      iFinal = (subtract 1) . unPos . sourceLine $ final
      jOg = (subtract 1) . unPos . sourceColumn $ og
      jFinal = (subtract 2) . unPos . sourceColumn $ final
   in (,) <$> [iOg .. iFinal] <*> [jOg .. jFinal]

-- IO/computations
filePath :: String
filePath = "src/Day03/data.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath
  let matrix = buildMatrix . lines $ inputStr
  let intMetas = fromRight [] $ runParser intMetasP "" inputStr
  let gearMetas = fromRight [] $ runParser gearMetasP "" inputStr

  let part1Answer = part1 matrix intMetas
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 intMetas gearMetas
  putStrLn $ "PART 2: " ++ show part2Answer

part1 :: Matrix Char -> [IntMeta] -> Int
part1 m =
  sum
    . map intVal
    . filter (isValidIntMeta m)

isValidIntMeta :: Matrix Char -> IntMeta -> Bool
isValidIntMeta m (IntMeta{coords}) =
  any (touchesSymbol m) coords

touchesSymbol :: Matrix Char -> Coord -> Bool
touchesSymbol m coord =
  let ns = getNeighborVals m coord
   in any
        ((&&) <$> (/= '.') <*> not . isDigit)
        ns

part2 :: [IntMeta] -> [GearMeta] -> Int
part2 intMetas =
  sum
    . mapMaybe (gearMetaRatio intMetas)

gearMetaRatio :: [IntMeta] -> GearMeta -> Maybe Int
gearMetaRatio intMetas gearMeta =
  let adj = adjacentIntMetas intMetas gearMeta
   in if length adj /= 2
        then Nothing
        else Just $ product (map intVal adj)

-- This isn't the most efficient...
-- We could use sets for intersection, for example
adjacentIntMetas :: [IntMeta] -> GearMeta -> [IntMeta]
adjacentIntMetas intMetas (GearMeta gCoord) =
  let nCoords = neighborCoords gCoord
   in filter
        (\IntMeta{coords} -> not . null $ intersect coords nCoords)
        intMetas
