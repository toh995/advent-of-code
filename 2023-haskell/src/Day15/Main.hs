module Day15.Main where

import Data.Char
import Data.Either
import Data.Foldable
import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as IntMap
import Data.List.Split (splitOn)
import Data.Void
import Text.Megaparsec hiding (label)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Label = String

data Lens = Lens
  { label :: Label
  , focalLength :: Int
  }
  deriving (Eq, Show)

data Command
  = Upsert Lens
  | Delete Label
  deriving (Show)

newtype Triple a = Triple (a, a, a)

instance Foldable Triple where
  foldr f z (Triple (a, b, c)) =
    f a (f b (f c z))

filePath :: String
filePath = "src/Day15/data.txt"

main :: IO ()
main = do
  inputStr <- readFile filePath
  let commands = parseCommands inputStr

  let part1Answer = part1 inputStr
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 commands
  putStrLn $ "PART 2: " ++ show part2Answer

part1 :: String -> Int
part1 =
  sum
    . map hash
    . splitOn ","
    . filter (/= '\n')

part2 :: [Command] -> Int
part2 cmds =
  let im = executeCommands cmds
   in sum
        . map (product . Triple)
        . concatMap
          ( \(i, lenses) ->
              zip3
                (repeat $ i + 1)
                [1 ..]
                (map focalLength lenses)
          )
        . IntMap.toList
        $ im

hash :: String -> Int
hash = foldl' f 0
 where
  f acc c =
    ((acc + ord c) * 17) `mod` 256

executeCommands :: [Command] -> IntMap [Lens]
executeCommands = foldl' f initialMap
 where
  f acc (Delete l) =
    let idx = hash l
     in IntMap.adjust
          (filter ((/= l) . label))
          idx
          acc
  f acc (Upsert lens) =
    let idx = hash . label $ lens
     in IntMap.adjust
          (upsert lens)
          idx
          acc
  initialMap =
    foldr
      (`IntMap.insert` [])
      IntMap.empty
      [0 .. 255]

upsert :: Lens -> [Lens] -> [Lens]
upsert lens ls =
  let (left, right) = span ((/= label lens) . label) ls
   in left ++ [lens] ++ drop 1 right

-- Parsing Logic
type Parser = Parsec Void String

parseCommands :: String -> [Command]
parseCommands =
  fromRight []
    . runParser commandsP ""

commandsP :: Parser [Command]
commandsP =
  commandP `sepBy` char ','

commandP :: Parser Command
commandP = try deleteP <|> try upsertP

deleteP :: Parser Command
deleteP =
  Delete
    <$> (many (noneOf [',', '-', '=', '\n']) <* char '-')

upsertP :: Parser Command
upsertP = do
  label <- many (noneOf [',', '-', '=', '\n'])
  _ <- char '='
  focalLength <- L.decimal
  let lens = Lens{label, focalLength}
  pure $ Upsert lens
