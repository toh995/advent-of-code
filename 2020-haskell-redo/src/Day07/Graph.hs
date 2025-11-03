{-# LANGUAGE NamedFieldPuns #-}

module Day07.Graph (Graph, fromList, dfs, inverse, nodes, weightedNeighbors) where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

newtype Graph a = Graph
    { adjList :: Map a (Set (a, Int))
    }

fromList :: (Ord a) => [(a, [(a, Int)])] -> Graph a
fromList list =
    Graph $
        M.fromListWith
            S.union
            [(node, S.fromList neighbors') | (node, neighbors') <- list]

inverse :: (Ord a) => Graph a -> Graph a
inverse Graph{adjList} =
    Graph{adjList = adjList'}
  where
    adjList' =
        adjList
            & M.toList
            & concatMap (\(from, tos) -> [(to, S.singleton (from, weight)) | (to, weight) <- S.toList tos])
            & M.fromListWith S.union

nodes :: (Ord a) => Graph a -> [a]
nodes Graph{adjList} =
    allNodes
        & S.fromList
        & S.toList
  where
    allNodes =
        M.keys adjList
            ++ concatMap (S.toList >>> map fst) (M.elems adjList)

weightedNeighbors :: (Ord a) => a -> Graph a -> [(a, Int)]
weightedNeighbors node Graph{adjList} =
    M.findWithDefault S.empty node adjList
        & S.toList

neighbors :: (Ord a) => a -> Graph a -> [a]
neighbors node graph =
    weightedNeighbors node graph
        & map fst

dfs :: (Ord a) => a -> Graph a -> [a]
dfs node graph =
    go [node] S.empty []
  where
    go [] _ result = result
    go (currNode : stack) seen result
        | currNode `S.member` seen = go stack seen result
        | otherwise =
            let
                stack' = neighbors currNode graph ++ stack
                seen' = currNode `S.insert` seen
                result' = currNode : result
             in
                go stack' seen' result'
