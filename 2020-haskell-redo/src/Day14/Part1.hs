{-# LANGUAGE NamedFieldPuns #-}

module Day14.Part1 where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
import Text.Parsec (Parsec, many1, parse, sepEndBy, try)
import qualified Text.Parsec.Char as C

data Bit = One | Zero
    deriving (Eq, Show)

-- Maps an "exponent" to a bit
newtype BitContainer = BitContainer {im :: IntMap Bit}
    deriving (Show)

data Instruction
    = SetMask BitContainer
    | SetMemory
        { address :: Integer
        , value :: BitContainer
        }
    deriving (Show)

data ProgramState = ProgramState
    { memory :: Map Integer BitContainer
    , mask :: Maybe BitContainer
    }

emptyState :: ProgramState
emptyState =
    ProgramState
        { memory = M.empty
        , mask = Nothing
        }

part1 :: String -> Either String Integer
part1 inputStr = do
    instructions <- first show $ parse instructionsP "" inputStr
    let finalState =
            foldl
                (flip doInstruction)
                emptyState
                instructions
    pure $
        memory finalState
            & M.map bitsToInteger
            & sum

doInstruction :: Instruction -> ProgramState -> ProgramState
doInstruction (SetMask mask) ps = ps{mask = Just mask}
doInstruction instruction@(SetMemory{}) ProgramState{mask = Nothing} =
    error $ "error executing instruction, attempted to set memory with an empty bitmask! " ++ show instruction
doInstruction SetMemory{address, value} ps@ProgramState{mask = Just maskVal, memory} =
    let value' = applyBitmask maskVal value
     in ps{memory = M.insert address value' memory}

bitsToInteger :: BitContainer -> Integer
bitsToInteger (BitContainer im) =
    sum
        [2 ^ i | (i, One) <- IM.toList im]

integerToBits :: Integer -> BitContainer
integerToBits num =
    BitContainer $
        go num 0 IM.empty
  where
    go n i im
        | n < 0 = error $ "cannot convert negative integers to bits, for integer " ++ show n
        | n == 0 = im
        | otherwise =
            let
                newBit = if odd n then One else Zero
                im' = IM.insert i newBit im
             in
                go (n `div` 2) (i + 1) im'

applyBitmask :: BitContainer -> BitContainer -> BitContainer
applyBitmask mask target =
    BitContainer
        { im = IM.union (im mask) (im target)
        }

type Parser = Parsec String ()

instructionsP :: Parser [Instruction]
instructionsP = sepEndBy instructionP (C.char '\n')

instructionP :: Parser Instruction
instructionP = try setMaskInstructionP <|> try setMemoryInstructionP

setMaskInstructionP :: Parser Instruction
setMaskInstructionP =
    C.string "mask = "
        >> bitmaskP
        <&> SetMask

setMemoryInstructionP :: Parser Instruction
setMemoryInstructionP = do
    _ <- C.string "mem["
    address <- integralP
    _ <- C.string "] = "
    value <- integralP <&> integerToBits
    pure $ SetMemory{address, value}

bitmaskP :: Parser BitContainer
bitmaskP = do
    bits <- many1 bitP
    let indexed = zip [0 ..] (reverse bits)
    let im = IM.fromList [(i, bit) | (i, Just bit) <- indexed]
    pure $ BitContainer im
  where
    bitP =
        (C.char '1' $> Just One)
            <|> (C.char '0' $> Just Zero)
            <|> (C.char 'X' $> Nothing)

integralP :: (Read a, Integral a) => Parser a
integralP =
    read <$> many1 C.digit
