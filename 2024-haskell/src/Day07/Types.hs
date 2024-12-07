module Day07.Types where

type Operand = Int

data Equation = Equation
  { answer :: Int
  , operands :: [Operand]
  }
  deriving (Show)
