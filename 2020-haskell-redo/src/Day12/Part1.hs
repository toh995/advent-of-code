{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Day12.Part1 where

import Control.Category ((>>>))
import qualified Data.Array as A
import Data.Foldable (find)
import Data.Function ((&))
import Data.Maybe (fromJust)

data Position = Position
    { coordPair :: (Int, Int)
    , direction :: Direction
    }

data Instruction
    = MoveAbsolute Direction Int
    | MoveRelative Int
    | Rotate Int

data Direction = North | South | East | West
    deriving (Eq)

part1 :: String -> Int
part1 inputStr =
    manhattanDistance
        (coordPair initialPosition)
        (coordPair finalPosition)
  where
    instructions = parseInstructions inputStr
    initialPosition =
        Position
            { coordPair = (0, 0)
            , direction = East
            }
    finalPosition =
        foldl
            (flip move)
            initialPosition
            instructions

parseInstructions :: String -> [Instruction]
parseInstructions = lines >>> map parseInstruction

parseInstruction :: String -> Instruction
parseInstruction [] = error "could not parse empty instruction"
parseInstruction (prefixChar : digitChars) =
    case prefixChar of
        'N' -> MoveAbsolute North num
        'S' -> MoveAbsolute South num
        'E' -> MoveAbsolute East num
        'W' -> MoveAbsolute West num
        'F' -> MoveRelative num
        'L' ->
            if
                | remainder == 0 -> Rotate (-numRotations)
                | otherwise -> error $ "could not parse rotation instruction " ++ (prefixChar : digitChars) ++ ": expected a multiple of 90"
        'R' ->
            if
                | remainder == 0 -> Rotate (numRotations)
                | otherwise -> error $ "could not parse rotation instruction " ++ (prefixChar : digitChars) ++ ": expected a multiple of 90"
        _ -> error $ "could not parse instruction " ++ (prefixChar : digitChars)
  where
    num = read digitChars
    (numRotations, remainder) = num `divMod` 90

move :: Instruction -> Position -> Position
move (MoveAbsolute dir magnitude) pos = pos{coordPair = translate dir magnitude (coordPair pos)}
move (MoveRelative magnitude) pos = pos{coordPair = translate (direction pos) magnitude (coordPair pos)}
move (Rotate numRotations) pos = pos{direction = rotate numRotations (direction pos)}

translate :: Direction -> Int -> (Int, Int) -> (Int, Int)
translate North dy (x, y) = (x, y + dy)
translate South dy (x, y) = (x, y - dy)
translate East dx (x, y) = (x + dx, y)
translate West dx (x, y) = (x - dx, y)

rotate :: Int -> Direction -> Direction
rotate numRotations =
    toIdx
        >>> (+ numRotations)
        >>> (`mod` 4)
        >>> (directions A.!)
  where
    directions = A.listArray (0, 3) [North, East, South, West]
    toIdx d =
        directions
            & A.indices
            & find (\i -> directions A.! i == d)
            & fromJust

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x, y) (x', y') =
    abs (x - x') + abs (y - y')
