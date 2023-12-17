module Day13.Main where

import Data.Functor
import Data.List
import Data.List.Split

filePath :: String
filePath = "src/Day13/data.txt"

main :: IO ()
main = do
  patterns <- readFile filePath <&> map lines . splitOn "\n\n"

  let part1Answer = sum . map getPatternScore1 $ patterns
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = sum . map getPatternScore2 $ patterns
  putStrLn $ "PART 2: " ++ show part2Answer

getPatternScore1 :: [[Char]] -> Int
getPatternScore1 rows =
  let cols = transpose rows
      rowPartitions =
        filter (uncurry isReflection)
          . map (`splitAt` rows)
          $ [1 .. length rows - 1]
      colPartitions =
        filter (uncurry isReflection)
          . map (`splitAt` cols)
          $ [1 .. length cols - 1]
      rowTotal = sum . map ((* 100) . length . fst) $ rowPartitions
      colTotal = sum . map (length . fst) $ colPartitions
   in rowTotal + colTotal

getPatternScore2 :: [[Char]] -> Int
getPatternScore2 rows =
  sum
    . nub
    . map (getPatternScoreForCoord rows)
    $ allCoords rows

getPatternScoreForCoord :: [[Char]] -> Coord -> Int
getPatternScoreForCoord rows (i, j) =
  let rows' = updateAtCoord changeSmudge rows (i, j)
      cols' = transpose rows'
      rowPartitions =
        filter (uncurry isReflection)
          . map (`splitAt` rows')
          $ [ max
              (i + 1 - (length rows' `div` 2))
              1
            .. min
              ((length rows' `div` 2) + i)
              (length rows' - 1)
            ]
      colPartitions =
        filter (uncurry isReflection)
          . map (`splitAt` cols')
          $ [ max
              (j + 1 - (length cols' `div` 2))
              1
            .. min
              ((length cols' `div` 2) + j)
              (length cols' - 1)
            ]
      rowTotal = sum . filter (/= getPatternScore1 rows) . map ((* 100) . length . fst) $ rowPartitions
      colTotal = sum . filter (/= getPatternScore1 rows) . map (length . fst) $ colPartitions
   in rowTotal + colTotal

isReflection :: (Eq a) => [[a]] -> [[a]] -> Bool
isReflection xss yss =
  let xss' = map reverse $ transpose xss
      yss' = transpose yss
   in all f $ zip xss' yss'
 where
  f (as, bs) =
    all (uncurry (==)) $ zip as bs

changeSmudge :: Char -> Char
changeSmudge '#' = '.'
changeSmudge '.' = '#'
changeSmudge c = c

type I = Int
type J = Int
type Coord = (I, J)

allCoords :: [[a]] -> [Coord]
allCoords [] = []
allCoords rs@(r : _) =
  (,) <$> [0 .. iMax] <*> [0 .. jMax]
 where
  iMax = length rs - 1
  jMax = length r - 1

updateAtCoord :: (a -> a) -> [[a]] -> Coord -> [[a]]
updateAtCoord f rows (i, j) =
  let (lRows, rRows) = splitAt i rows
      currRow = head rRows
      (lCols, rCols) = splitAt j currRow
      currCell = head rCols
      currCell' = f currCell
      currRow' = lCols ++ [currCell'] ++ drop 1 rCols
   in lRows ++ [currRow'] ++ drop 1 rRows
