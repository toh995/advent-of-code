module Day06.Main where

import Data.Either (fromRight)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Time = Int
type Distance = Int

data Race = Race
  { totalTime :: Time
  , targetDistance :: Distance
  }
  deriving (Show)

filePath :: String
filePath = "src/Day06/data.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath

  let races1 = parseRaces1 inputStr
  let part1Answer = computeAnswer races1
  putStrLn $ "PART 1: " ++ show part1Answer

  let races2 = parseRaces2 inputStr
  let part2Answer = computeAnswer races2
  putStrLn $ "PART 2: " ++ show part2Answer

computeAnswer :: [Race] -> Int
computeAnswer = product . map numWaysToWin

numWaysToWin :: Race -> Int
numWaysToWin (Race{totalTime, targetDistance}) =
  let distances =
        map
          (getDistance totalTime)
          [0 .. totalTime]
   in length
        . filter (> targetDistance)
        $ distances

getDistance :: Time -> Time -> Distance
getDistance totalTime holdTime =
  let travelTime = totalTime - holdTime
      speed = holdTime
   in travelTime * speed

-- Parsing Logic
type Parser = Parsec Void String

parseRaces1 :: String -> [Race]
parseRaces1 =
  fromRight []
    . runParser racesP1 ""

racesP1 :: Parser [Race]
racesP1 = do
  times <- string "Time:" *> hspace *> L.decimal `sepBy` hspace <* newline
  distances <- string "Distance:" *> hspace *> L.decimal `sepBy` hspace
  return $ zipWith Race times distances

parseRaces2 :: String -> [Race]
parseRaces2 =
  fromRight []
    . runParser racesP2 ""
    . filter (/= ' ')

racesP2 :: Parser [Race]
racesP2 = do
  totalTime <- string "Time:" *> L.decimal <* newline
  targetDistance <- string "Distance:" *> L.decimal
  return [Race{totalTime, targetDistance}]
