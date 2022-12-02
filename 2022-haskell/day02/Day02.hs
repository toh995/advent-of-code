{-# OPTIONS_GHC -Wall #-}

module Day02 where

import Data.Maybe

-------------------------------
-- Data types and helper fns --
-------------------------------
data Move
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Show)

data Round = Round
  { myMove :: Move,
    oppMove :: Move,
    result :: RoundResult
  }
  deriving (Show)

data RoundResult
  = Win
  | Draw
  | Loss
  deriving (Show)

-- given a move m, return the move that will beat m
winner :: Move -> Move
winner Rock = Paper
winner Paper = Scissors
winner Scissors = Rock

-- given a move m, return the move that will lose to m
loser :: Move -> Move
loser Rock = Scissors
loser Paper = Rock
loser Scissors = Paper

-- Given two moves, get the result
getResult :: Move -> Move -> RoundResult
getResult myMove oppMove
  | myMove == winner oppMove = Win
  | myMove == loser oppMove = Loss
  | otherwise = Draw

-- Deduce what my move would be,
-- given the result and oppMove
getMyMove :: RoundResult -> Move -> Move
getMyMove Draw oppMove = oppMove
getMyMove Win oppMove = winner oppMove
getMyMove Loss oppMove = loser oppMove

----------------------
-- SECTION: Main IO --
----------------------
fileName :: String
fileName = "data.txt"

main :: IO ()
main = do
  inputStr <- readFile fileName

  let rounds1 = parse1 inputStr
  let part1Answer = getScore rounds1
  putStrLn $ "PART 1: " ++ show part1Answer

  let rounds2 = parse2 inputStr
  let part2Answer = getScore rounds2
  putStrLn $ "PART 2: " ++ show part2Answer

------------------------------
-- SECTION: Parsing, part 1 --
------------------------------
type Line = String

parse1 :: String -> [Round]
parse1 = mapMaybe parseLine1 . lines

parseLine1 :: Line -> Maybe Round
parseLine1 [oppChar, ' ', meChar] =
  let myMove = parseMe meChar
      oppMove = parseOpp oppChar
      result = getResult <$> myMove <*> oppMove
   in Round <$> myMove <*> oppMove <*> result
parseLine1 _ = Nothing

parseOpp :: Char -> Maybe Move
parseOpp 'A' = Just Rock
parseOpp 'B' = Just Paper
parseOpp 'C' = Just Scissors
parseOpp _ = Nothing

parseMe :: Char -> Maybe Move
parseMe 'X' = Just Rock
parseMe 'Y' = Just Paper
parseMe 'Z' = Just Scissors
parseMe _ = Nothing

------------------------------
-- SECTION: Parsing, part 2 --
------------------------------
parse2 :: String -> [Round]
parse2 = mapMaybe parseLine2 . lines

parseLine2 :: Line -> Maybe Round
parseLine2 [oppChar, ' ', resultChar] =
  let oppMove = parseOpp oppChar
      result = parseResult resultChar
      myMove = getMyMove <$> result <*> oppMove
   in Round <$> myMove <*> oppMove <*> result
parseLine2 _ = Nothing

parseResult :: Char -> Maybe RoundResult
parseResult 'X' = Just Loss
parseResult 'Y' = Just Draw
parseResult 'Z' = Just Win
parseResult _ = Nothing

----------------------
-- SECTION: Scoring --
----------------------
getScore :: [Round] -> Int
getScore rounds = winLossScore + myMoveScore
  where
    winLossScore = sum . map (resultVal . result) $ rounds
    myMoveScore = sum . map (moveVal . myMove) $ rounds

resultVal :: RoundResult -> Int
resultVal Win = 6
resultVal Draw = 3
resultVal Loss = 0

moveVal :: Move -> Int
moveVal Rock = 1
moveVal Paper = 2
moveVal Scissors = 3
