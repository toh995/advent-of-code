{-# LANGUAGE NamedFieldPuns #-}

module Day14.Part2 where

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

data Bit = One | Zero | Floating
    deriving (Eq, Show)

-- Maps an "exponent" to a bit
type BitContainer = IntMap Bit

data Instruction
    = SetMask BitContainer
    | SetMemory
        { address :: Integer
        , value :: Integer
        }
    deriving (Show)

data ProgramState = ProgramState
    { memory :: Map Integer Integer
    , mask :: Maybe BitContainer
    }

emptyState :: ProgramState
emptyState =
    ProgramState
        { memory = M.empty
        , mask = Nothing
        }

part2 :: String -> Either String Integer
part2 inputStr = do
    instructions <- first show $ parse instructionsP "" inputStr
    let finalState =
            foldl
                (flip doInstruction)
                emptyState
                instructions
    pure $
        memory finalState
            & M.elems
            & sum

doInstruction :: Instruction -> ProgramState -> ProgramState
doInstruction (SetMask mask) ps = ps{mask = Just mask}
doInstruction instruction@(SetMemory{}) ProgramState{mask = Nothing} =
    error $ "error executing instruction, attempted to set memory with an empty bitmask! " ++ show instruction
doInstruction SetMemory{address, value} ps@ProgramState{mask = Just maskVal} =
    let addresses = applyBitmask maskVal address
     in foldr
            (\address' ps' -> ps'{memory = M.insert address' value (memory ps')})
            ps
            addresses

applyBitmask :: BitContainer -> Integer -> [Integer]
applyBitmask mask n =
    bitsToInteger <$> finalBitContainers
  where
    initialBitContainers = [integerToBits n]
    finalBitContainers =
        foldr f initialBitContainers (IM.toList mask)
    f (_, Zero) acc = acc
    f (i, One) acc = IM.insert i One <$> acc
    f (i, Floating) acc =
        concat
            [ [ IM.insert i One im
              , IM.insert i Zero im
              ]
            | im <- acc
            ]

bitsToInteger :: BitContainer -> Integer
bitsToInteger bitContainer =
    sum
        [2 ^ i | (i, One) <- IM.toList bitContainer]

integerToBits :: Integer -> BitContainer
integerToBits num =
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
    value <- integralP
    pure $ SetMemory{address, value}

bitmaskP :: Parser BitContainer
bitmaskP = do
    bits <- many1 bitP
    pure $ IM.fromList $ zip [0 ..] (reverse bits)

bitP :: Parser Bit
bitP =
    (C.char '1' $> One)
        <|> (C.char '0' $> Zero)
        <|> (C.char 'X' $> Floating)

integralP :: (Read a, Integral a) => Parser a
integralP =
    read <$> many1 C.digit
