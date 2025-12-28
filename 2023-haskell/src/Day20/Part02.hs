module Day20.Part02 where

import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Text.Parsec (Parsec, choice, many, many1, optionMaybe, parse, try)
import Text.Parsec.Char qualified as C

type NodeId = String

data Graph = Graph
    { adjList :: Map NodeId [NodeId]
    , revAdjList :: Map NodeId [NodeId]
    , nodeTypes :: Map NodeId NodeType
    }

newGraph :: Map NodeId [NodeId] -> Map NodeId NodeType -> Graph
newGraph adjList nodeTypes =
    Graph
        { adjList
        , nodeTypes
        , revAdjList
        }
  where
    revAdjList =
        M.fromListWith
            (++)
            [ (to, [from])
            | (from, tos) <- M.assocs adjList
            , to <- tos
            ]

outNeighbors :: NodeId -> Graph -> [NodeId]
outNeighbors node Graph{adjList} = M.findWithDefault [] node adjList

inNeighbors :: NodeId -> Graph -> [NodeId]
inNeighbors node Graph{revAdjList} = M.findWithDefault [] node revAdjList

data NodeType = FlipFlop | Conjunction

data NodeState
    = FlipFlopState OnOff
    | ConjunctionState {prevPulses :: Map NodeId Pulse}

data OnOff = On | Off

data Pulse = HighPulse | LowPulse
    deriving (Eq)

part2 :: String -> Int
part2 inputStr =
    foldr1 lcm
        . M.elems
        $ firstHighPulseSentRound targetNodes graph
  where
    graph =
        unwrapEither $
            parse graphP "" inputStr
    targetNodes = inNeighbors "gf" graph

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Left a) = error $ show a
unwrapEither (Right b) = b

data RoundState = RoundState
    { roundNum :: Int
    , nodeStates :: Map NodeId NodeState
    , targetNodeSet :: Set NodeId
    , ret :: Map NodeId Int
    }

firstHighPulseSentRound :: [NodeId] -> Graph -> Map NodeId Int
firstHighPulseSentRound targetNodes graph =
    ret finalState
  where
    finalState =
        until
            (S.null . targetNodeSet)
            doRound
            initState
    initState =
        RoundState
            { roundNum = 0
            , nodeStates = initNodeStates graph
            , targetNodeSet = S.fromList targetNodes
            , ret = M.empty
            }
    doRound rs = go startQueue rs{roundNum = roundNum rs + 1}
      where
        startQueue =
            Seq.fromList
                [ ("broadcaster", LowPulse, neighbor)
                | neighbor <- outNeighbors "broadcaster" graph
                ]

        go Empty currRS = currRS
        go ((fromNode, pulse, toNode) :<| queue) currRS =
            go (queue >< nextQueue) currRS'
          where
            (nextQueue, nodeStates') = applyPulse (fromNode, pulse, toNode) (nodeStates currRS)
            currRS' =
                currRS{nodeStates = nodeStates'}
                    & recordCycleIfNeeded fromNode pulse

        applyPulse (fromNode, pulse, toNode) currNodeStates =
            fromMaybe
                (Seq.empty, currNodeStates)
                $ do
                    nodeState <- currNodeStates M.!? toNode
                    (nextPulse, nextNodeState) <- processPulse fromNode pulse nodeState
                    let nextQueue =
                            Seq.fromList
                                [ (toNode, nextPulse, neighbor)
                                | neighbor <- outNeighbors toNode graph
                                ]
                    let nextNodeStates = M.insert toNode nextNodeState currNodeStates
                    pure (nextQueue, nextNodeStates)

        recordCycleIfNeeded fromNode pulse currRS
            | pulse == HighPulse && fromNode `S.member` targetNodeSet currRS =
                currRS
                    { targetNodeSet = S.delete fromNode (targetNodeSet currRS)
                    , ret = M.insert fromNode (roundNum currRS) (ret currRS)
                    }
            | otherwise = currRS

processPulse :: NodeId -> Pulse -> NodeState -> Maybe (Pulse, NodeState)
processPulse _ HighPulse (FlipFlopState _) = Nothing
processPulse _ LowPulse (FlipFlopState Off) = Just (HighPulse, FlipFlopState On)
processPulse _ LowPulse (FlipFlopState On) = Just (LowPulse, FlipFlopState Off)
processPulse fromNode pulse (ConjunctionState{prevPulses}) =
    Just
        ( nextPulse
        , ConjunctionState{prevPulses = prevPulses'}
        )
  where
    prevPulses' =
        prevPulses
            & M.insert fromNode pulse
    nextPulse =
        if prevPulses'
            & M.elems
            & all (== HighPulse)
            then LowPulse
            else HighPulse

initNodeStates :: Graph -> Map NodeId NodeState
initNodeStates graph =
    M.mapWithKey
        initNodeState
        (nodeTypes graph)
  where
    initNodeState _ FlipFlop = FlipFlopState Off
    initNodeState node Conjunction =
        ConjunctionState
            { prevPulses =
                M.fromList
                    [ (fromNode, LowPulse)
                    | fromNode <- inNeighbors node graph
                    ]
            }

type Parser = Parsec String ()

graphP :: Parser Graph
graphP = do
    (nodeTypePairs, adjListPairs) <- unzip <$> sepBy1 rowP C.newline
    let nodeTypePairs' = catMaybes nodeTypePairs
    pure $
        newGraph
            (M.fromList adjListPairs)
            (M.fromList nodeTypePairs')

rowP :: Parser (Maybe (NodeId, NodeType), (NodeId, [NodeId]))
rowP = do
    nodeTypeM <- optionMaybe nodeTypeP
    fromNode <- nodeIdP
    _ <- C.string " -> "
    toNodes <- sepBy1 nodeIdP (C.string ", ")
    pure
        ( (fromNode,) <$> nodeTypeM
        , (fromNode, toNodes)
        )

nodeTypeP :: Parser NodeType
nodeTypeP =
    choice
        [ try (FlipFlop <$ C.char '%')
        , try (Conjunction <$ C.char '&')
        ]

nodeIdP :: Parser NodeId
nodeIdP = many1 $ try C.letter

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 aP sepP = do
    a <- aP
    as <- many (try $ sepP >> aP)
    pure (a : as)
