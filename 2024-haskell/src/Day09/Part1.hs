module Day09.Part1 where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.State.Lazy
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Deque.Lazy (Deque)
import Deque.Lazy qualified as Deque
import GHC.Exts
import Prelude hiding (appendFile)

import Day09.Types

part1 :: [Block] -> Int
part1 = checksum . compress

-- Remove all FreeSpace blocks, using the "compression algorithm"
-- specified in the problem statement
compress :: [Block] -> [Block]
compress inputBlocks =
  toList finalS.result
 where
  finalS :: S
  finalS =
    (`execState` initialS)
      . evalContT
      $ callCC
        ( \exit -> do
            advanceLeftBlock exit
            advanceCompressFile exit
            forever $ processBlock exit
        )
  initialS =
    S
      { result = Deque.fromConsAndSnocLists [] []
      , unprocessedFileIds = HashSet.fromList . map (\f -> f.id) . filter isFileBlock $ inputBlocks
      , currLeftBlock = FreeSpace{size = 0}
      , leftBlockQueue = inputBlocks
      , compressFilesQueue = reverse . filter isFileBlock $ inputBlocks
      , currCompressFile = File{id = -1, size = 0}
      }

data S = S
  { result :: Deque Block
  , unprocessedFileIds :: HashSet Int
  , currLeftBlock :: Block
  , leftBlockQueue :: [Block]
  , compressFilesQueue :: [Block]
  , currCompressFile :: Block
  }

processBlock :: (MonadState S m, MonadCont m) => (() -> m ()) -> m ()
processBlock exit = do
  s <- get
  let allFilesProcessed = HashSet.null s.unprocessedFileIds
  when allFilesProcessed (exit ())

  let hasFileOverlap =
        case (s.currLeftBlock, s.currCompressFile) of
          (f1@File{}, f2@File{}) -> f1.id == f2.id
          _ -> False
  when
    hasFileOverlap
    $ appendToResult s.currCompressFile >> exit ()

  case s.currLeftBlock of
    file@File{} ->
      appendToResult file
        >> markAsProcessed file
        >> advanceLeftBlock exit
    free@FreeSpace{} ->
      do
        let currFile = s.currCompressFile
        let appendFile = currFile{size = min currFile.size free.size}
        appendToResult appendFile
        if
          | free.size > currFile.size ->
              do
                markAsProcessed currFile
                advanceCompressFile exit
                let free' = free{size = free.size - currFile.size}
                modify $ \s' -> s'{currLeftBlock = free'}
          | free.size < currFile.size ->
              do
                let currFile' = currFile{size = currFile.size - free.size}
                modify $ \s' -> s'{currCompressFile = currFile'}
                advanceLeftBlock exit
          | otherwise ->
              do
                markAsProcessed currFile
                advanceCompressFile exit
                advanceLeftBlock exit

appendToResult :: (MonadState S m) => Block -> m ()
appendToResult b = modify $ \s -> s{result = Deque.snoc b s.result}

markAsProcessed :: (MonadState S m) => Block -> m ()
markAsProcessed file@File{} = modify $ \s -> s{unprocessedFileIds = HashSet.delete file.id s.unprocessedFileIds}
markAsProcessed _ = pure ()

advanceLeftBlock :: (MonadState S m, MonadCont m) => (() -> m ()) -> m ()
advanceLeftBlock exit =
  get >>= \s ->
    case s.leftBlockQueue of
      [] -> exit ()
      (b : bs) ->
        put
          s
            { currLeftBlock = b
            , leftBlockQueue = bs
            }

advanceCompressFile :: (MonadState S m, MonadCont m) => (() -> m ()) -> m ()
advanceCompressFile exit =
  get >>= \s ->
    case s.compressFilesQueue of
      [] -> exit ()
      (b : bs) ->
        put
          s
            { currCompressFile = b
            , compressFilesQueue = bs
            }
