module Day09.Types where

import Data.List
import Prelude hiding (id)

data Block
  = File {id, size :: Int}
  | FreeSpace {size :: Int}
  deriving (Eq, Show)

isFileBlock :: Block -> Bool
isFileBlock File{} = True
isFileBlock _ = False

checksum :: [Block] -> Int
checksum = checksum' 0
 where
  checksum' :: Int -> [Block] -> Int
  checksum' _ [] = 0
  checksum' i (free@FreeSpace{} : bs) = checksum' (i + free.size) bs
  checksum' i (file@File{} : bs) =
    addend + checksum' i' bs
   where
    i' = i + file.size
    addend = file.id * sum [i .. i' - 1]

sortByFileIdAsc :: [Block] -> [Block]
sortByFileIdAsc =
  sortBy
    ( \b1 b2 -> case (b1, b2) of
        (f1@File{}, f2@File{})
          | f1.id < f2.id -> LT
          | f1.id > f2.id -> GT
          | otherwise -> EQ
        _ -> EQ
    )

mergeAdjacentFree :: [Block] -> [Block]
mergeAdjacentFree (f1@FreeSpace{} : f2@FreeSpace{} : rest) =
  mergeAdjacentFree (merged : rest)
 where
  merged = FreeSpace{size = f1.size + f2.size}
mergeAdjacentFree (b : bs) = b : mergeAdjacentFree bs
mergeAdjacentFree [] = []

showBlocks :: [Block] -> String
showBlocks = concatMap showBlock

showBlock :: Block -> String
showBlock File{id, size} = concat . replicate size $ show id
showBlock FreeSpace{size} = replicate size '.'
