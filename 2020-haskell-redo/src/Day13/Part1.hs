module Day13.Part1 where

import Control.Category ((>>>))
import Data.Foldable (minimumBy)
import Data.Function (on, (&))
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Text.Read (readMaybe)

type BusId = Int
type Timestamp = Int

part1 :: String -> Int
part1 inputStr =
    minBusId * waitTime
  where
    (timestamp, busIds) = parse inputStr
    minBusId =
        minimumBy
            (compare `on` (earliestDeparture timestamp))
            busIds
    waitTime = earliestDeparture timestamp minBusId - timestamp

parse :: String -> (Timestamp, [BusId])
parse s =
    case lines s of
        [line1, line2] ->
            let
                timestamp = read line1
                busIds =
                    line2
                        & splitOn ","
                        & mapMaybe readMaybe
             in
                (timestamp, busIds)
        _ -> error "expected two lines in the input"

splitOn :: String -> String -> [String]
splitOn delim =
    T.pack
        >>> T.splitOn (T.pack delim)
        >>> map (T.unpack)

earliestDeparture :: Timestamp -> BusId -> Timestamp
earliestDeparture timestamp busId
    | rem' == 0 = busId * quot'
    | otherwise = busId * (quot' + 1)
  where
    (quot', rem') = timestamp `divMod` busId
