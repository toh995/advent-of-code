module Day04.Main where

import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Char (isDigit)
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Text.Read (readMaybe)

filePath :: String
filePath = "src/Day04/data.txt"

main :: IO ()
main = do
    inputStr <- readFile filePath
    let passportStrings = splitOn "\n\n" inputStr
    let passports = parsePassport <$> passportStrings

    let part1Answer = part1 passports
    putStrLn ("PART 1: " ++ show part1Answer)

    let part2Answer = part2 passports
    putStrLn ("PART 2: " ++ show part2Answer)

part1 :: [Passport] -> Int
part1 = filter isValid1 >>> length

part2 :: [Passport] -> Int
part2 = filter isValid2 >>> length

isValid1 :: Passport -> Bool
isValid1 passport =
    all
        (`M.member` passport)
        requiredFields
  where
    requiredFields =
        [ "byr"
        , "iyr"
        , "eyr"
        , "hgt"
        , "hcl"
        , "ecl"
        , "pid"
        ]

isValid2 :: Passport -> Bool
isValid2 passport =
    case validation of
        Just () -> True
        Nothing -> False
  where
    validation = do
        byr <- passport M.!? "byr" >>= readMaybe
        guard $ 1920 <= byr && byr <= (2002 :: Int)

        iyr <- passport M.!? "iyr" >>= readMaybe
        guard $ 2010 <= iyr && iyr <= (2020 :: Int)

        eyr <- passport M.!? "eyr" >>= readMaybe
        guard $ 2020 <= eyr && eyr <= (2030 :: Int)

        (hgtAmount, hgtUnit) <- passport M.!? "hgt" >>= parseHeight
        guard $
            case hgtUnit of
                Cm -> 150 <= hgtAmount && hgtAmount <= 193
                In -> 59 <= hgtAmount && hgtAmount <= 76

        hcl <- passport M.!? "hcl"
        let validHclChars = S.fromList $ ['0' .. '9'] ++ ['a' .. 'f']
        guard $
            length hcl == 7
                && hcl !! 0 == '#'
                && ( hcl
                        & drop 1
                        & all (`S.member` validHclChars)
                   )

        ecl <- passport M.!? "ecl"
        let validEcls = S.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        guard $ ecl `S.member` validEcls

        pid <- passport M.!? "pid"
        guard $ length pid == 9 && all isDigit pid

data HeightUnit = Cm | In

parseHeight :: String -> Maybe (Int, HeightUnit)
parseHeight s = do
    let (numStr, unitStr) = span isDigit s
    num <- readMaybe numStr
    heightUnit <- parseHeightUnit unitStr
    pure (num, heightUnit)

parseHeightUnit :: String -> Maybe HeightUnit
parseHeightUnit "cm" = Just Cm
parseHeightUnit "in" = Just In
parseHeightUnit _ = Nothing

type Passport = Map String String

parsePassport :: String -> Map String String
parsePassport s =
    M.fromList kvPairs
  where
    kvPairs =
        s
            & words
            & map (split1 ':')

split1 :: Char -> String -> (String, String)
split1 delim s =
    (prefix, drop 1 suffix)
  where
    (prefix, suffix) = span (/= delim) s

splitOn :: String -> String -> [String]
splitOn delim s =
    s
        & T.pack
        & T.splitOn (T.pack delim)
        & map (T.unpack)
