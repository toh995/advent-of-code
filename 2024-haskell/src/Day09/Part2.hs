module Day09.Part2 where

import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace
import Safe

import Day09.Types

part2 :: [Block] -> Int
part2 = checksum . compress

compress :: [Block] -> [Block]
compress inputBlocks =
  foldr f inputBlocks sortedFiles
 where
  sortedFiles =
    sortByFileIdAsc
      . filter isFileBlock
      $ inputBlocks
  f FreeSpace{} accum = accum
  f file@File{} accum =
    fromMaybe accum $
      do
        -- Find an empty slot to the left of the file
        iFile <- elemIndex file accum
        iFree <-
          findIndex
            ( \case
                free@FreeSpace{} -> free.size >= file.size
                _ -> False
            )
            accum
        guard $ iFree < iFile
        -- Insert file into the new position
        free <- atMay accum iFree
        let free' = free{size = free.size - file.size}
        let (l, r) = splitAt iFree accum
        let accum' = l ++ [file, free'] ++ drop 1 r
        -- Insert FreeSpace in the old position, accounting for the new offset.
        -- We need to merge adjacent FreeSpaces.
        let (l', r') = splitAt iFile accum'
        let blocksToMerge =
              catMaybes
                [ headMay r'
                , pure FreeSpace{size = file.size}
                , headMay $ drop 2 r'
                ]
        let merged = mergeAdjacentFree blocksToMerge
        pure $ l' ++ merged ++ drop 3 r'
