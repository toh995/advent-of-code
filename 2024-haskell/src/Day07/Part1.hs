module Day07.Part1 where

import Control.Monad.Except
import Data.Either

import Day07.Types

part1 :: [Equation] -> Int
part1 =
  sum
    . map answer
    . filter canResolve

data Operator = Add | Mul
  deriving (Eq, Show)

canResolve :: Equation -> Bool
canResolve Equation{answer, operands} =
  elem answer
    . rights
    . map (evaluate operands)
    $ permuteOperators (length operands - 1)

permuteOperators :: Int -> [[Operator]]
permuteOperators n
  | n > 0 = (:) <$> [Add, Mul] <*> permuteOperators (n - 1)
  | n == 0 = [[]]
  | otherwise = []

evaluate :: (MonadError String m) => [Operand] -> [Operator] -> m Int
evaluate (x : xs) ops = pure . foldl f x $ zip xs ops
 where
  f :: Int -> (Operand, Operator) -> Int
  f accum (n, op) =
    case op of
      Add -> accum + n
      Mul -> accum * n
evaluate _ _ = throwError "Could not evaluate: need at least one operand, got 0."
