{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day12.Part2 where

import Control.Category ((>>>))

data Instruction
    = MoveWaypoint {delta :: (Int, Int)}
    | MoveShip {magnitude :: Int}
    | RotateWaypoint
        { direction :: RotateDirection
        , numRotations :: Int
        }

data RotateDirection = Clockwise | CounterClockwise

data Position = Position
    { shipCoord :: (Int, Int)
    , waypointCoord :: (Int, Int)
    }

part2 :: String -> Int
part2 inputStr =
    manhattanDistance
        (shipCoord startPosition)
        (shipCoord finalPosition)
  where
    instructions = parseInstructions inputStr
    startPosition =
        Position
            { shipCoord = (0, 0)
            , waypointCoord = (10, 1)
            }
    finalPosition =
        foldl
            (flip move)
            startPosition
            instructions

parseInstructions :: String -> [Instruction]
parseInstructions = lines >>> map parseInstruction

parseInstruction :: String -> Instruction
parseInstruction [] = error "could not parse empty instruction"
parseInstruction (prefixChar : digitChars) =
    case prefixChar of
        'N' -> MoveWaypoint{delta = (0, num)}
        'S' -> MoveWaypoint{delta = (0, -num)}
        'E' -> MoveWaypoint{delta = (num, 0)}
        'W' -> MoveWaypoint{delta = (-num, 0)}
        'F' -> MoveShip{magnitude = num}
        'L' ->
            if
                | remainder == 0 -> RotateWaypoint{direction = CounterClockwise, numRotations}
                | otherwise -> error $ "could not parse rotation instruction " ++ (prefixChar : digitChars) ++ ": expected a multiple of 90"
        'R' ->
            if
                | remainder == 0 -> RotateWaypoint{direction = Clockwise, numRotations}
                | otherwise -> error $ "could not parse rotation instruction " ++ (prefixChar : digitChars) ++ ": expected a multiple of 90"
        _ -> error $ "could not parse instruction " ++ (prefixChar : digitChars)
  where
    num = read digitChars
    (numRotations, remainder) = num `divMod` 90

move :: Instruction -> Position -> Position
move MoveWaypoint{delta} pos@Position{waypointCoord} =
    pos{waypointCoord = translate waypointCoord delta}
move MoveShip{magnitude} pos@Position{shipCoord} =
    pos{shipCoord = translate shipCoord delta}
  where
    delta = scalarMult magnitude (waypointCoord pos)
move RotateWaypoint{direction, numRotations} pos@Position{waypointCoord} =
    pos
        { waypointCoord =
            applyN numRotations (rotate direction) waypointCoord
        }

translate :: (Int, Int) -> (Int, Int) -> (Int, Int)
translate (x, y) (dx, dy) = (x + dx, y + dy)

scalarMult :: Int -> (Int, Int) -> (Int, Int)
scalarMult n (x, y) = (n * x, n * y)

rotate :: RotateDirection -> (Int, Int) -> (Int, Int)
rotate CounterClockwise (x, y) = (-y, x)
rotate Clockwise (x, y) = (y, -x)

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n - 1) f (f x)

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x, y) (x', y') =
    abs (x - x') + abs (y - y')
