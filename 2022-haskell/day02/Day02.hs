{-# OPTIONS_GHC -Wall #-}

module Day02 where

-------------------------------
-- Data types and helper fns --
-------------------------------
data Move
  = NilMove
  | Rock
  | Paper
  | Scissors
  deriving (Eq, Show)

data Round
  = NilRound
  | Round
      { myMove :: Move,
        oppMove :: Move,
        result :: RoundResult
      }
  deriving (Show)

data RoundResult
  = NilResult
  | Win
  | Draw
  | Loss
  deriving (Show)

-- given a move m, return the move that will beat m
winner :: Move -> Move
winner Rock = Paper
winner Paper = Scissors
winner Scissors = Rock
winner NilMove = NilMove

-- given a move m, return the move that will lose to m
loser :: Move -> Move
loser Rock = Scissors
loser Paper = Rock
loser Scissors = Paper
loser NilMove = NilMove

-- Given two moves, get the result
getResult :: Move -> Move -> RoundResult
getResult myMove oppMove
  | myMove == NilMove = NilResult
  | oppMove == NilMove = NilResult
  | myMove == winner oppMove = Win
  | myMove == loser oppMove = Loss
  | otherwise = Draw

-- Deduce what my move would be,
-- given the result and oppMove
getMyMove :: RoundResult -> Move -> Move
getMyMove NilResult _ = NilMove
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
  let part1Answer = totalScore rounds1
  putStrLn $ "PART 1: " ++ show part1Answer

  let rounds2 = parse2 inputStr
  let part2Answer = totalScore rounds2
  putStrLn $ "PART 2: " ++ show part2Answer

------------------------------
-- SECTION: Parsing, part 1 --
------------------------------
parse1 :: String -> [Round]
parse1 = map parseLine1 . lines

parseLine1 :: String -> Round
parseLine1 [oppChar, _, meChar] =
  let myMove = parseMe meChar
      oppMove = parseOpp oppChar
      result = getResult myMove oppMove
   in Round {myMove, oppMove, result}
parseLine1 _ = NilRound

parseOpp :: Char -> Move
parseOpp 'A' = Rock
parseOpp 'B' = Paper
parseOpp 'C' = Scissors
parseOpp _ = NilMove

parseMe :: Char -> Move
parseMe 'X' = Rock
parseMe 'Y' = Paper
parseMe 'Z' = Scissors
parseMe _ = NilMove

------------------------------
-- SECTION: Parsing, part 2 --
------------------------------
parse2 :: String -> [Round]
parse2 = map parseLine2 . lines

parseLine2 :: String -> Round
parseLine2 [oppChar, _, resultChar] =
  let oppMove = parseOpp oppChar
      result = parseResult resultChar
      myMove = getMyMove result oppMove
   in Round {myMove, oppMove, result}
parseLine2 _ = NilRound

parseResult :: Char -> RoundResult
parseResult 'X' = Loss
parseResult 'Y' = Draw
parseResult 'Z' = Win
parseResult _ = NilResult

-- given the round result and the opponent's move,
-- deduce what my move would be
parseMe2 :: RoundResult -> Move -> Move
parseMe2 NilResult _ = NilMove
parseMe2 Draw oppMove = oppMove
parseMe2 Win oppMove = winner oppMove
parseMe2 Loss oppMove = loser oppMove

----------------------
-- SECTION: Scoring --
----------------------
totalScore :: [Round] -> Int
totalScore rounds = winLossScore + myMoveScore
  where
    winLossScore = sum . map (resultVal . result) $ rounds
    myMoveScore = sum . map (moveVal . myMove) $ rounds

resultVal :: RoundResult -> Int
resultVal Win = 6
resultVal Draw = 3
resultVal Loss = 0
resultVal NilResult = 0

moveVal :: Move -> Int
moveVal Rock = 1
moveVal Paper = 2
moveVal Scissors = 3
moveVal NilMove = 0
