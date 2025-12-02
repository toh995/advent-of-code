{-# LANGUAGE NamedFieldPuns #-}

module Day01.Main where

import Control.Category ((>>>))
import Data.Functor ((<&>))

type Position = Int

data Command = Command
    { direction :: Direction
    , magnitude :: Int
    }

data Direction = L | R

filePath :: FilePath
filePath = "src/Day01/data.txt"

main :: IO ()
main = do
    commands <-
        readFile filePath
            <&> ( lines
                    >>> map parseCommand
                )
    let part1Answer = part1 commands
    putStrLn $ "PART1: " ++ show part1Answer
    let part2Answer = part2 commands
    putStrLn $ "PART2: " ++ show part2Answer

part1 :: [Command] -> Int
part1 =
    scanl (flip newPosition) initialPos
        >>> filter (== 0)
        >>> length
  where
    initialPos = 50

part2 :: [Command] -> Int
part2 commands =
    go initialPos commands 0
  where
    initialPos = 50
    go _ [] acc = acc
    go currPos (command@Command{direction, magnitude} : cs) acc =
        go nextPos cs (acc + moreAcc)
      where
        nextPos = newPosition command currPos
        moreAcc
            | currPos == 0 = magnitude `div` 100
            | (magnitude `mod` 100) >= ((sign * currPos) `mod` 100) = (magnitude `div` 100) + 1
            | otherwise = magnitude `div` 100
        sign =
            case direction of
                L -> 1
                R -> -1

newPosition :: Command -> Position -> Position
newPosition Command{direction, magnitude} pos =
    case direction of
        L -> (pos - magnitude) `mod` 100
        R -> (pos + magnitude) `mod` 100

parseCommand :: String -> Command
parseCommand ('L' : digits) = Command{direction = L, magnitude = read digits}
parseCommand ('R' : digits) = Command{direction = R, magnitude = read digits}
parseCommand s = error $ "could not parse command; invalid input string " ++ s
