module Day12.Main where

import Data.Either
import Data.Function.Memoize
import Data.Functor
import Data.List
import Data.Void
import Safe
import Text.Megaparsec hiding (label)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

filePath :: String
filePath = "src/Day12/data.txt"

main :: IO ()
main = do
  parsed <- readFile filePath <&> parseAll

  let part1Answer = part1 parsed
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 parsed
  putStrLn $ "PART 2: " ++ show part2Answer

part1 :: [([Char], [Int])] -> Int
part1 =
  sum
    . map (uncurry countValidArrangementsM)

part2 :: [([Char], [Int])] -> Int
part2 =
  sum
    . map (uncurry countValidArrangementsM . unravel)

countValidArrangementsM :: [Char] -> [Int] -> Int
countValidArrangementsM = memoize2 countValidArrangements

countValidArrangements :: [Char] -> [Int] -> Int
countValidArrangements [] [] = 1
countValidArrangements [] (_ : _) = 0
countValidArrangements cs []
  | '#' `elem` cs = 0
  | otherwise = 1
countValidArrangements (c : cs) (n : ns)
  -- The line is not long enough
  | length (c : cs) < sum (n : ns) + length (n : ns) - 1 = 0
  | c == '.' = countValidArrangementsM (dropWhile (== '.') cs) (n : ns)
  | c == '#' =
      let (left, right) = splitAt n (c : cs)
       in if
            | length left /= n -> 0
            | '.' `elem` left -> 0
            | headMay right == Just '#' -> 0
            | otherwise -> countValidArrangementsM (drop 1 right) ns
  | c == '?' =
      countValidArrangementsM ('#' : cs) (n : ns)
        + countValidArrangementsM ('.' : cs) (n : ns)
  | otherwise = 0

unravel :: ([Char], [Int]) -> ([Char], [Int])
unravel (cs, ns) =
  (cs', ns')
 where
  cs' = intercalate "?" . replicate 5 $ cs
  ns' = concat . replicate 5 $ ns

-- Parsing logic
type Parser = Parsec Void String

parseAll :: String -> [([Char], [Int])]
parseAll =
  fromRight []
    . runParser linesP ""

linesP :: Parser [([Char], [Int])]
linesP = lineP `sepEndBy` newline

lineP :: Parser ([Char], [Int])
lineP =
  (,)
    <$> (many (oneOf ".?#") <* char ' ')
    <*> (L.decimal `sepBy` char ',')
