module Day05 where

import Data.Char
import Data.List
import Data.Maybe

-------------------------
-- SECTION: Data Types --
-------------------------
type Line = String

data Crate = Crate {crateChar :: Char}
  deriving (Show)

type Stack = [Crate]

type FromStack = Stack

type ToStack = Stack

data Config = Config {getStacks :: [Stack]}
  deriving (Show)

data Instruction = Instruction
  { amount :: Int,
    fromIdx :: Int,
    toIdx :: Int
  }
  deriving (Show)

type TransferFn = Int -> (FromStack, ToStack) -> (FromStack, ToStack)

----------------------
-- SECTION: Main IO --
----------------------
filePath :: String
filePath = "data/Day05.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath
  let ls = lines inputStr
  let config = buildConfig . getConfigLines $ ls
  let instructions = buildInstructions . getInstructionLines $ ls

  let part1Answer = part1 config instructions
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 config instructions
  putStrLn $ "PART 2: " ++ show part2Answer

-----------------------------
-- SECTION: Config Parsing --
-----------------------------
getConfigLines :: [Line] -> [Line]
getConfigLines =
  map cleanConfigLine
    . takeWhile (\l -> not . isDigit $ l !! 1)

-- remove extra square brackets and spaces
-- we make a big assumption here in terms of the spacing of letters
cleanConfigLine :: Line -> Line
cleanConfigLine (_ : c : cs) = c : takeEveryNth 4 cs
cleanConfigLine line = line

buildConfig :: [Line] -> Config
buildConfig rows =
  let cols = transpose rows
      cleanCols = map (filter isAlpha) cols
      stacks = map buildStack cleanCols
   in Config stacks

buildStack :: [Char] -> Stack
buildStack = map Crate

----------------------------------
-- SECTION: Instruction Parsing --
----------------------------------
getInstructionLines :: [Line] -> [Line]
getInstructionLines = drop 1 . dropWhile (/= "")

buildInstructions :: [Line] -> [Instruction]
buildInstructions = mapMaybe buildInstruction

buildInstruction :: Line -> Maybe Instruction
buildInstruction line =
  case (words line) of
    ["move", a, "from", from, "to", to] ->
      Just $
        Instruction (read a) ((read from) - 1) ((read to) - 1)
    _ -> Nothing

----------------------------
-- SECTION: Parts 1 and 2 --
----------------------------
part1 :: Config -> [Instruction] -> String
part1 c is = compute transfer1 c is

transfer1 :: Int -> (FromStack, ToStack) -> (FromStack, ToStack)
transfer1 n (from, to) =
  let newFrom = drop n from
      newTo = reverse (take n from) ++ to
   in (newFrom, newTo)

part2 :: Config -> [Instruction] -> String
part2 c is = compute transfer2 c is

transfer2 :: Int -> (FromStack, ToStack) -> (FromStack, ToStack)
transfer2 n (from, to) =
  let newFrom = drop n from
      newTo = take n from ++ to
   in (newFrom, newTo)

-------------------------
-- SECTION: Core Logic --
-------------------------
compute :: TransferFn -> Config -> [Instruction] -> String
compute transferFn config instructions =
  let finalConfig = applyInstructions transferFn config instructions
      topCrates = getTopCrates finalConfig
      serializedAnswer = map crateChar topCrates
   in serializedAnswer

applyInstructions :: TransferFn -> Config -> [Instruction] -> Config
applyInstructions transferFn config is =
  foldl' (applyInstruction transferFn) config is

applyInstruction :: TransferFn -> Config -> Instruction -> Config
applyInstruction transferFn config (Instruction {fromIdx, toIdx, amount}) =
  let from = findStack fromIdx config
      to = findStack toIdx config
      (newFrom, newTo) = transferFn amount (from, to)
   in (updateConfig fromIdx newFrom)
        . (updateConfig toIdx newTo)
        $ config

findStack :: Int -> Config -> Stack
findStack i (Config stacks) = stacks !! i

updateConfig :: Int -> Stack -> Config -> Config
updateConfig i new (Config stacks) =
  Config newStacks
  where
    newStacks = fromMaybe stacks (updateAt i new stacks)

getTopCrates :: Config -> [Crate]
getTopCrates (Config stacks) = mapMaybe listToMaybe stacks

-------------------------
-- SECTION: List Utils --
-------------------------
takeEveryNth :: Int -> [a] -> [a]
takeEveryNth n xs =
  case (drop (n - 1) xs) of
    (x : rest) -> x : takeEveryNth n rest
    _ -> []

updateAt :: Int -> a -> [a] -> Maybe [a]
updateAt i x xs
  | i < 0 = Nothing
  | i >= length xs = Nothing
  | otherwise = Just $ take i xs ++ [x] ++ drop (i + 1) xs
