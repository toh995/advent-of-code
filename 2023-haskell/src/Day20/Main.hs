module Day20.Main where

import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import Data.Either
import Data.Functor
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.Maybe
import Debug.Trace

-- import Data.Sequence
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Label = String
type SourceLabel = Label
type DestLabel = Label

newtype Graph = Graph (HashMap Label [Label])
  deriving (Show)

newtype Modules = Modules (HashMap Label Module)
  deriving (Eq, Show)

data Pulse = High | Low
  deriving (Eq, Show)

data Status = On | Off
  deriving (Eq, Show)

data Module
  = FlipFlop !Status
  | Conjunction !(HashMap Label Pulse)
  | Broadcast
  deriving (Eq, Show)

data Counter = Counter
  { high, low :: Int
  }
  deriving (Eq, Show)

instance Semigroup Counter where
  c1 <> c2 =
    Counter
      { high = high c1 + high c2
      , low = low c1 + low c2
      }

instance Monoid Counter where
  mempty = Counter{high = 0, low = 0}

increment :: Pulse -> Counter -> Counter
increment High c = c{high = high c + 1}
increment Low c = c{low = low c + 1}

emptyGraph :: Graph
emptyGraph = Graph mempty

getNeighborLabs :: Label -> Graph -> Maybe [Label]
getNeighborLabs lab (Graph hm) =
  HashMap.lookup lab hm

getInputLabs :: Label -> Graph -> [Label]
getInputLabs targetLab (Graph hm) =
  map fst
    . filter (elem targetLab . snd)
    . HashMap.toList
    $ hm

isFlipFlop :: Module -> Bool
isFlipFlop (FlipFlop _) = True
isFlipFlop _ = False

isConjunction :: Module -> Bool
isConjunction (Conjunction _) = True
isConjunction _ = False

emptyModules :: Modules
emptyModules = Modules mempty

getModule :: Label -> Modules -> Maybe Module
getModule lab (Modules hm) =
  HashMap.lookup lab hm

insertMod :: Label -> Module -> Modules -> Modules
insertMod lab m (Modules hm) =
  Modules $ HashMap.insert lab m hm

initializeFlipFlops :: Modules -> Modules
initializeFlipFlops (Modules hm) =
  let keys = map fst . filter (isFlipFlop . snd) . HashMap.toList $ hm
      hm' = HashMap.fromList $ (,FlipFlop Off) <$> keys
   in Modules $ hm' <> hm

initializeConjunctions :: Graph -> Modules -> Modules
initializeConjunctions g (Modules hm) =
  let keys = map fst . filter (isConjunction . snd) . HashMap.toList $ hm
      vals = keys <&> initialConjunctionVal g
      hm' = HashMap.fromList $ zip keys vals
   in Modules $ hm' <> hm

initialConjunctionVal :: Graph -> Label -> Module
initialConjunctionVal g lab =
  Conjunction
    ( HashMap.fromList
        . map (,Low)
        $ getInputLabs lab g
    )

-- foo :: Module -> Pulse -> Label -> (Module, Pulse)
-- foo (FlipFlop s) High _ = (FlipFlop s, High)
-- foo (FlipFlop Off) Low _ = (FlipFlop On, High)
-- foo (FlipFlop On) Low _ = (FlipFlop Off, Low)
-- foo (Conjunction hm) p prevMod
--   | all (== High) . HashMap.elems $ hm' = (Conjunction hm', Low)
--   | otherwise = (Conjunction hm', High)
--  where
--   hm' = HashMap.insert prevMod p hm
-- foo Broadcast p _ = (Broadcast, p)

{-
b lo -> & => ALWAYS HIGH
b lo -> % => 0, 2, 4,... High  0 + 2k
          => 1, 3, 5,...  Low  1 + 2k

b lo -> % -> % => 1, 5, 9, ... High 1 + (2k - 1)
               => 3, 7, 11,... Low  3 + (2k - 1)
& hi -> & => sometimes low
& hi -> % => NOTHING
-}

getNextPulse :: Module -> Pulse -> Maybe Pulse
getNextPulse Broadcast p = Just p
getNextPulse (FlipFlop _) High = Nothing
getNextPulse (FlipFlop On) Low = Just High
getNextPulse (FlipFlop Off) Low = Just Low
getNextPulse (Conjunction hm) _
  | all (== High) . HashMap.elems $ hm = Just Low
  | otherwise = Just High

getNextModule :: Module -> Pulse -> SourceLabel -> Module
getNextModule Broadcast _ _ = Broadcast
getNextModule (FlipFlop s) High _ = FlipFlop s
getNextModule (FlipFlop Off) Low _ = FlipFlop On
getNextModule (FlipFlop On) Low _ = FlipFlop Off
getNextModule (Conjunction hm) p sourceLab =
  Conjunction $
    HashMap.insert sourceLab p hm

{-
Memoize
  - Modules => count, next state
-}

-- foo :: Graph -> Modules -> (Counter, Modules)
-- foo g mods =

countPulses1000 :: Graph -> Modules -> Counter
countPulses1000 g initialMods =
  let initialS = S{mods = initialMods, counter = mempty, lowSentToRx = False}
      S{counter} =
        execState
          ( replicateM
              1000
              (countPulsesS g [(Low, "button", "broadcaster")])
          )
          initialS
   in counter

-- in fromMaybe mempty (finalSM <&> counter)

data S = S
  { mods :: !Modules
  , counter :: !Counter
  , lowSentToRx :: !Bool
  }

-- @fixme double check the failure conditions
-- countPulsesS :: Graph -> [(Label, Pulse)] -> StateT S Maybe ()
-- countPulsesS _ [] = pure ()
-- countPulsesS g ((lab, pulse) : queue) = do
--   s <- get
--   -- Increment the counter
--   put $ s{counter = increment pulse (counter s)}
--
--   -- Update the module state
--   m <- lift $ getModule lab (mods s)
--   let m' = getNextModule m pulse lab
--   let mods' = insertMod lab m' (mods s)
--   put $ s{mods = mods'}
--
--   -- Enqueue the neighbors
--   neighborLabs <- lift $ getNeighborLabs lab g
--   let pulseM' = getNextPulse m' pulse
--   let queue' =
--         case pulseM' of
--           Nothing -> []
--           Just pulse' -> (,pulse') <$> neighborLabs
--   countPulsesS g (queue ++ queue')

-- mapMaybe (\n -> ) neighbors

-- case getNextPulse m' pulse of
--   Nothing -> pure ()
--   (Just pulse') ->
--     let queue' = queue ++ map (,pulse') neighbors
--      in fooS g queue'

-- @fixme: Don't fail all for the Nothing case
-- pulse' <- lift $ getNextPulse m' pulse
-- let queue' = queue ++ map (,pulse') neighbors
-- fooS g queue'

-- pure ()
countPulsesS ::
  Graph ->
  [(Pulse, SourceLabel, DestLabel)] ->
  State S ()
countPulsesS _ [] = pure ()
countPulsesS g ((pulse, sourceLab, lab) : queue) =
  modify
    ( \s ->
        if lab == "rx" && pulse == Low
          then s{lowSentToRx = True}
          else s
    )
    >> modify (\s -> s{counter = increment pulse (counter s)})
    >> do
      nextQueueM <- runMaybeT $ do
        s <- lift get
        -- Update the module state
        m <- hoistMaybe $ getModule lab (mods s)
        let m' = getNextModule m pulse sourceLab
        lift $ put s{mods = insertMod lab m' (mods s)}
        -- Find the next queue items
        pulse' <- hoistMaybe $ getNextPulse m' pulse
        neighborLabs <- hoistMaybe $ getNeighborLabs lab g
        pure $ (pulse',lab,) <$> neighborLabs

      -- Enqueue the new pulses/label combos
      let nextQueue = fromMaybe [] nextQueueM
      -- let queue' = trace (show (queue ++ nextQueue)) (queue ++ nextQueue)
      -- countPulsesS g queue'
      countPulsesS g (queue ++ nextQueue)

-------------
-- Main IO --
-------------
filePath :: String
filePath = "src/Day20/data.txt"

main :: IO ()
main = do
  (graph, modules) <- readFile filePath <&> parseAll

  let part1Answer = part1 graph modules
  putStrLn $ "PART 1: " ++ show part1Answer

  let part2Answer = part2 graph modules
  putStrLn $ "PART 2: " ++ show part2Answer

part1 :: Graph -> Modules -> Int
part1 g mods =
  let initialS = S{mods, counter = mempty, lowSentToRx = False}
      S{counter} =
        execState
          ( replicateM
              1000
              (countPulsesS g [(Low, "button", "broadcaster")])
          )
          initialS
   in low counter * high counter

part2 :: Graph -> Modules -> Int
part2 g mods =
  let initialS = S{mods, counter = mempty, lowSentToRx = False}
      rounds =
        evalState
          ( untilM
              (countPulsesS g [(Low, "button", "broadcaster")])
              (get <&> lowSentToRx)
          )
          initialS
   in length rounds

-- let initialS = S{mods = modules, counter = mempty}
-- let S{counter} =
--       execState
--         (countPulsesS graph [(Low, "button", "broadcaster")])
--         initialS
-- print counter

-------------------
-- Parsing Logic --
-------------------
type Parser = Parsec Void String

parseAll :: String -> (Graph, Modules)
parseAll =
  fromRight (emptyGraph, emptyModules)
    . runParser allP ""

allP :: Parser (Graph, Modules)
allP = do
  ls <- lineP `sepEndBy` newline
  let graph = Graph . HashMap.fromList . map snd $ ls
  let mods = Modules . HashMap.fromList . map fst $ ls
  let mods' =
        initializeConjunctions graph
          . initializeFlipFlops
          $ mods
  pure (graph, mods')

lineP :: Parser ((Label, Module), (Label, [Label]))
lineP = do
  m <- moduleP
  lab <- (labelP <|> pure "broadcaster") <* string " -> "
  neighborLabs <- labelP `sepBy` string ", "
  pure ((lab, m), (lab, neighborLabs))

moduleP :: Parser Module
moduleP =
  (char '%' $> FlipFlop Off)
    <|> (char '&' $> Conjunction mempty)
    <|> (string "broadcaster" $> Broadcast)

labelP :: Parser Label
labelP = some letterChar

-----------------
-- Other Utils --
-----------------
hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure
