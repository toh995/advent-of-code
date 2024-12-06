module Day03.Main where

import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Data.Functor
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

filePath :: String
filePath = "src/Day03/data.txt"

main :: IO ()
main =
  runExceptT main' >>= \case
    Left err -> error err
    _ -> pure ()

main' :: (MonadIO m, MonadError String m) => m ()
main' = do
  cmds <- parseCmds =<< liftIO (readFile filePath)
  let part1Answer = part1 cmds
  liftIO $ putStrLn ("PART 1: " ++ show part1Answer)
  let part2Answer = part2 cmds
  liftIO $ putStrLn ("PART 2: " ++ show part2Answer)

data Cmd
  = Mult Int Int
  | Do
  | Dont
  deriving (Show)

part1 :: [Cmd] -> Int
part1 =
  sum
    . mapMaybe
      ( \case
          Mult a b -> Just $ a * b
          _ -> Nothing
      )

part2 :: [Cmd] -> Int
part2 = part1 . filterByDoDont

data CmdState = MultEnabled | MultDisabled

filterByDoDont :: [Cmd] -> [Cmd]
filterByDoDont cmds =
  evalState
    (filterByDoDontS cmds)
    MultEnabled

filterByDoDontS :: (MonadState CmdState m) => [Cmd] -> m [Cmd]
filterByDoDontS [] = pure []
filterByDoDontS (Do : cmds) = put MultEnabled >> filterByDoDontS cmds
filterByDoDontS (Dont : cmds) = put MultDisabled >> filterByDoDontS cmds
filterByDoDontS (m@(Mult _ _) : cmds) =
  get >>= \case
    MultEnabled -> (m :) <$> filterByDoDontS cmds
    MultDisabled -> filterByDoDontS cmds

-------------------
-- PARSING LOGIC --
-------------------
type MonadParser = MonadParsec Void String

parseCmds :: (MonadError String m) => String -> m [Cmd]
parseCmds =
  modifyError show
    . liftEither
    . runParser (ignoreP *> cmdsP) ""

cmdsP :: (MonadParser m) => m [Cmd]
cmdsP = sepEndBy cmdP ignoreP

cmdP :: (MonadParser m) => m Cmd
cmdP = multP <|> doP <|> dontP

multP :: (MonadParser m) => m Cmd
multP = try m
 where
  m = do
    _ <- string "mul("
    a <- L.decimal
    _ <- char ','
    b <- L.decimal
    _ <- char ')'
    pure $ Mult a b

doP :: (MonadParser m) => m Cmd
doP = try (string "do()") $> Do

dontP :: (MonadParser m) => m Cmd
dontP = try (string "don't()") $> Dont

ignoreP :: (MonadParser m) => m ()
ignoreP =
  skipManyTill anySingle (eof <|> (lookAhead cmdP $> ()))
