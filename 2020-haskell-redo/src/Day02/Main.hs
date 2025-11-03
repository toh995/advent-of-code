{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day02.Main where

import Control.Category ((>>>))
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- import qualified Data.Array.IArray as A
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UA
import Data.Function ((&))
import Data.Maybe (catMaybes)
import Text.Parsec (ParseError, Parsec, anyToken, many1, parse, sepEndBy)
import qualified Text.Parsec.Char as C
import Prelude hiding (lines)

type Password = String

data Policy = Policy
    { num1 :: Int
    , num2 :: Int
    , char :: Char
    }
    deriving (Show)

filePath :: String
filePath = "src/Day02/data.txt"

main :: IO ()
main =
    runExceptT main'
        >>= \case
            Right () -> pure ()
            Left errMsg -> error $ show errMsg

main' :: (MonadIO m, MonadError ParseError m) => m ()
main' = do
    inputStr <- liftIO $ readFile filePath
    lines <- liftEither $ parse linesP filePath inputStr

    let part1Answer = part1 lines
    liftIO $ putStrLn ("PART 1: " ++ show part1Answer)

    let part2Answer = part2 lines
    liftIO $ putStrLn ("PART 2: " ++ show part2Answer)

part1 :: [(Policy, Password)] -> Int
part1 =
    filter (uncurry isValid1)
        >>> length

isValid1 :: Policy -> Password -> Bool
isValid1 Policy{num1, num2, char} password =
    num1 <= actualCount && actualCount <= num2
  where
    actualCount =
        password
            & filter (== char)
            & length

part2 :: [(Policy, Password)] -> Int
part2 =
    filter (uncurry isValid2)
        >>> length

isValid2 :: Policy -> Password -> Bool
isValid2 Policy{num1, num2, char} password =
    [ passwordArr UA.!? num1
    , passwordArr UA.!? num2
    ]
        & catMaybes
        & filter (== char)
        & length
        & (== 1)
  where
    passwordArr = UA.listArray (1, length password) password :: UArray Int Char

linesP :: Parsec String () [(Policy, Password)]
linesP = sepEndBy lineP (C.char '\n')

lineP :: Parsec String () (Policy, Password)
lineP = do
    policy <- policyP
    _ <- C.string ": "
    password <- many1 $ C.noneOf "\n"
    pure (policy, password)

policyP :: Parsec String () Policy
policyP = do
    num1 <- intP
    _ <- C.char '-'
    num2 <- intP
    _ <- C.char ' '
    char <- anyToken
    pure Policy{num1, num2, char}

intP :: Parsec String () Int
intP = read <$> many1 C.digit
