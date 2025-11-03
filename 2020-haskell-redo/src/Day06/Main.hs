module Day06.Main where

import Control.Category ((>>>))
import Data.Function ((&))
import qualified Data.Set as S
import qualified Data.Text as T

filePath :: String
filePath = "src/Day06/data.txt"

main :: IO ()
main = do
    inputStr <- readFile filePath
    let groups =
            inputStr
                & splitOn "\n\n"
                & map lines

    let part1Answer = part1 groups
    putStrLn ("PART 1: " ++ show part1Answer)
    let part2Answer = part2 groups
    putStrLn ("PART 2: " ++ show part2Answer)

part1 :: [[String]] -> Int
part1 =
    map questionCount >>> sum
  where
    questionCount = map S.fromList >>> S.unions >>> S.size

part2 :: [[String]] -> Int
part2 =
    map questionCount >>> sum
  where
    questionCount =
        map S.fromList
            >>> foldr1 S.intersection
            >>> S.size

splitOn :: String -> String -> [String]
splitOn delim s =
    s
        & T.pack
        & T.splitOn (T.pack delim)
        & map T.unpack
