{-# LANGUAGE LambdaCase #-}

module Day08.Main where

import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import Data.Foldable (find)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.IntSet as IS
import Data.Maybe (fromJust)
import Text.Read (readEither)

data Instruction
    = Acc Int
    | Jmp Int
    | Nop Int

filePath :: String
filePath = "src/Day08/data.txt"

main :: IO ()
main = do
    inputStr <- readFile filePath
    let instructions =
            case parseInstructions inputStr of
                Right val -> val
                Left errMsg -> error errMsg

    let part1Answer = part1 instructions
    putStrLn ("PART 1: " ++ show part1Answer)

    let part2Answer = part2 instructions
    putStrLn ("PART 2: " ++ show part2Answer)

part1 :: Array Int Instruction -> Int
part1 instructions =
    case runProgram instructions of
        Cycle acc -> acc
        Terminated _ -> error "huh??"

part2 :: Array Int Instruction -> Int
part2 instructions =
    swappedArrs
        & map runProgram
        & find
            ( \case
                Terminated _ -> True
                Cycle _ -> False
            )
        & fromJust
        & resultVal
  where
    accJmpIndexes =
        instructions
            & A.indices
            & filter
                ( \i -> case instructions A.! i of
                    Acc _ -> False
                    Jmp _ -> True
                    Nop _ -> True
                )
    swappedArrs =
        accJmpIndexes
            & map
                ( \i ->
                    let
                        instruction = instructions A.! i
                     in
                        instructions A.// [(i, swap instruction)]
                )
    swap (Acc n) = Acc n
    swap (Jmp n) = Nop n
    swap (Nop n) = Jmp n

data ProgramResult
    = Cycle {resultVal :: Int}
    | Terminated {resultVal :: Int}

runProgram :: Array Int Instruction -> ProgramResult
runProgram instructions =
    go 0 0 IS.empty
  where
    (_, lastI) = A.bounds instructions
    go i acc seen
        | i `IS.member` seen = Cycle acc
        | i == lastI + 1 = Terminated acc
        | otherwise =
            case instructions A.! i of
                Acc dAcc -> go (i + 1) (acc + dAcc) (i `IS.insert` seen)
                Jmp di -> go (i + di) acc (i `IS.insert` seen)
                Nop _ -> go (i + 1) acc (i `IS.insert` seen)

parseInstructions :: String -> Either String (Array Int Instruction)
parseInstructions s = do
    instructionList <- mapM parseInstruction $ lines s
    pure $
        A.listArray
            (0, length instructionList - 1)
            instructionList

parseInstruction :: String -> Either String Instruction
parseInstruction s =
    case splitAt 4 s of
        ("acc ", intStr) -> Acc <$> parseInt intStr
        ("jmp ", intStr) -> Jmp <$> parseInt intStr
        ("nop ", intStr) -> Nop <$> parseInt intStr
        _ -> Left $ "Invalid instruction '" ++ "'"

parseInt :: String -> Either String Int
parseInt ('+' : digitChars) = readEither digitChars
parseInt ('-' : digitChars) = readEither digitChars <&> (* (-1))
parseInt s = Left $ "invalid integer string '" ++ s ++ "'"
