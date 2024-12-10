module Day09.Parse (parseBlocks) where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.List
import Text.Read hiding (get)

import Day09.Types

type Counter = Int
initialCount :: Counter
initialCount = 0

parseBlocks :: (MonadError String m) => String -> m [Block]
parseBlocks str = evalStateT (blocksS . filter (/= '\n') $ str) initialCount

blocksS :: (MonadError String m, MonadState Counter m) => String -> m [Block]
blocksS (c1 : c2 : cs) = (++) <$> sequence [fileS c1, freeSpaceS c2] <*> blocksS cs
blocksS [c] = singleton <$> fileS c
blocksS [] = pure []

fileS :: (MonadError String m, MonadState Counter m) => Char -> m Block
fileS c = do
  size <- liftEither . readEither $ [c]
  fileId <- get
  modify (+ 1)
  pure File{size, id = fileId}

freeSpaceS :: (MonadError String m) => Char -> m Block
freeSpaceS c =
  FreeSpace
    <$> (liftEither . readEither $ [c])
