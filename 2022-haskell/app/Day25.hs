module Day25 where

import Data.Maybe

type Line = String


filePath :: String
filePath = "data/Day25.txt"

main :: IO ()
main = do
  ls <- lines <$> readFile filePath

  let part1Answer = part1 ls
  putStrLn $ "PART 1: " ++ show part1Answer

part1 :: [Line] -> String
part1 = decimalToSnafu
      . sum
      . map snafuToDecimal

snafuToDecimal :: String -> Integer
snafuToDecimal chars =
  let decimals = mapMaybe snafuCharToDecimal chars
      (ret, _) = foldr
                   (\n (acc, factor) -> (acc + (n*factor), factor*5))
                   (0, 1)
                   decimals
   in ret

decimalToSnafu :: Integer -> String
decimalToSnafu n
  | n == 0 = ['0'] 
  | otherwise = f n [] 1
  where
    f :: Integer -> String -> Integer -> String
    f num acc factor
      | num == 0 = acc
      | otherwise =
          let snafuChar = fromJust $ decimalToSnafuChar ((num `div` factor) `mod` 5)
              charDecimal = fromJust $ snafuCharToDecimal snafuChar
              num' = num - (factor * charDecimal)
              acc' = snafuChar : acc
              factor' = factor * 5
           in f num' acc' factor'
        
decimalToSnafuChar :: Integer -> Maybe Char
decimalToSnafuChar 0 = Just '0'
decimalToSnafuChar 1 = Just '1'
decimalToSnafuChar 2 = Just '2'
decimalToSnafuChar 3 = Just '='
decimalToSnafuChar 4 = Just '-'
decimalToSnafuChar _ = Nothing

snafuCharToDecimal :: Char -> Maybe Integer
snafuCharToDecimal '2' = Just 2
snafuCharToDecimal '1' = Just 1
snafuCharToDecimal '0' = Just 0
snafuCharToDecimal '-' = Just (-1)
snafuCharToDecimal '=' = Just (-2)
snafuCharToDecimal _   = Nothing
