module Day07.Part2 where

import Control.Monad.Except
import Data.Either

import Day07.Types

part2 :: [Equation] -> Int
part2 =
  sum
    . map answer
    . filter canResolve

data Operator = Add | Mul | Concat
  deriving (Eq, Show)

canResolve :: Equation -> Bool
canResolve Equation{answer, operands} =
  elem answer
    . rights
    . map (evaluate operands)
    $ permuteOperators (length operands - 1)

permuteOperators :: Int -> [[Operator]]
permuteOperators n
  | n > 0 = (:) <$> [Add, Mul, Concat] <*> permuteOperators (n - 1)
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
      Concat -> read $ show accum ++ show n
evaluate _ _ = throwError "Could not evaluate: need at least one operand, got 0."
