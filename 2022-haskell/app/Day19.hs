module Day19 where

import Data.Either
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable
import Data.Void
import Safe.Foldable
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

----------------------
-- SECTION: Main IO --
----------------------
filePath :: String
filePath = "data/Day19.txt"

main :: IO ()
main = do
  blueprints <- parseBlueprints . lines <$> readFile filePath

  let part1Answer = part1 blueprints
  putStrLn $ "PART 1: " ++ show part1Answer
  
  let part2Answer = part2 blueprints
  putStrLn $ "PART 2: " ++ show part2Answer

----------------------
-- SECTION: Parsing --
----------------------
type Line = String

type Parser a = Parsec Void String a

parseBlueprints :: [Line] -> [Blueprint]
parseBlueprints = rights . map (runParser blueprintParser "")

blueprintParser :: Parser Blueprint
blueprintParser = do
  blueprintId <- string "Blueprint " *> L.decimal
  _ <- string ": "

  -- oreRobotCostParser

  let parsers = [
                  oreRobotCostParser,
                  clayRobotCostParser,
                  obsidianRobotCostParser,
                  geodeRobotCostParser
                ]
  costMap <- HashMap.fromList <$> sequence parsers
  return Blueprint {blueprintId, costMap}

oreRobotCostParser :: Parser (RobotType, HashMap Resource Integer)
oreRobotCostParser = do
  _ <- string "Each ore robot costs "
  oreCount <- L.decimal
  _ <- string " ore. "
  let hm = HashMap.singleton Ore oreCount
  return (OreCollector, hm)

clayRobotCostParser :: Parser (RobotType, HashMap Resource Integer)
clayRobotCostParser = do
  _ <- string "Each clay robot costs "
  oreCount <- L.decimal
  _ <- string " ore. "
  let hm = HashMap.singleton Ore oreCount
  return (ClayCollector, hm)

obsidianRobotCostParser :: Parser (RobotType, HashMap Resource Integer)
obsidianRobotCostParser = do
  _ <- string "Each obsidian robot costs "
  oreCount <- L.decimal
  _ <- string " ore and "
  clayCount <- L.decimal
  _ <- string " clay. "
  let hm = HashMap.fromList [(Ore, oreCount), (Clay, clayCount)]
  return (ObsidianCollector, hm)

geodeRobotCostParser :: Parser (RobotType, HashMap Resource Integer)
geodeRobotCostParser = do
  _ <- string "Each geode robot costs "
  oreCount <- L.decimal
  _ <- string " ore and "
  obsidianCount <- L.decimal
  _ <- string " obsidian"
  let hm = HashMap.fromList [(Ore, oreCount), (Obsidian, obsidianCount)]
  return (GeodeCracker, hm)

----------------------------------
-- SECTION: Resource, RobotType --
----------------------------------
data Resource
  = Obsidian
  | Clay
  | Ore
  | Geode
  deriving (Bounded, Enum, Eq, Show)

instance Hashable Resource where
  hashWithSalt n resource = hashWithSalt n $ fromEnum resource
  hash resource = hash . fromEnum $ resource

allResources :: [Resource]
allResources = enumFrom minBound

data RobotType
  = ObsidianCollector
  | ClayCollector
  | OreCollector
  | GeodeCracker
  deriving (Bounded, Enum, Eq, Show)

instance Hashable RobotType where
  hashWithSalt n robotType = hashWithSalt n . fromEnum $ robotType
  hash robotType = hash . fromEnum $ robotType

allRobotTypes :: [RobotType]
allRobotTypes = enumFrom minBound

robotTypeFor :: Resource -> RobotType
robotTypeFor Obsidian = ObsidianCollector
robotTypeFor Clay = ClayCollector
robotTypeFor Ore = OreCollector
robotTypeFor Geode = GeodeCracker

resourceFor :: RobotType -> Resource
resourceFor ObsidianCollector = Obsidian
resourceFor ClayCollector = Clay
resourceFor OreCollector = Ore
resourceFor GeodeCracker = Geode

------------------------
-- SECTION: Blueprint --
------------------------
data Blueprint = Blueprint
  { blueprintId :: Integer,
    costMap :: HashMap RobotType (HashMap Resource Integer)
  }
  deriving (Show)

getCost :: Blueprint -> RobotType -> Resource -> Integer
getCost (Blueprint {costMap}) robotType resource =
  let innerMap = HashMap.findWithDefault HashMap.empty robotType costMap
   in HashMap.findWithDefault 0 resource innerMap

requiredResources :: Blueprint -> RobotType -> [Resource]
requiredResources blueprint robotType =
  filter
    ((> 0) <$> getCost blueprint robotType)
    allResources

getMaxRobotCount :: Blueprint -> RobotType -> Integer
getMaxRobotCount (Blueprint {costMap}) robotType =
  let resource = resourceFor robotType
   in maximumBound 0 $
        HashMap.map (HashMap.findWithDefault 0 resource) costMap

----------------------
-- SECTION: Counter --
----------------------
data Counter a = Hashable a => Counter {
  counterHashMap :: (HashMap a Integer)
}

getCount :: a -> Counter a -> Integer
getCount a (Counter hm) = HashMap.findWithDefault 0 a hm

updateCount :: (Integer -> Integer) -> a -> Counter a -> Counter a
updateCount f a counter@(Counter hm) =
  let currCount = getCount a counter
      newCount = f currCount
      updated = HashMap.insert a newCount hm
   in Counter updated

-------------------------
-- SECTION: RoundState --
-------------------------
data RoundState = RoundState
  { minutesLeft :: Integer,
    robotCounter :: Counter RobotType,
    resourceCounter :: Counter Resource
  }

initialRoundState :: Integer -> RoundState
initialRoundState minutesLeft =
  RoundState
    { minutesLeft,
      robotCounter = Counter $ HashMap.singleton OreCollector 1,
      resourceCounter = Counter HashMap.empty
    }

getRobotCount :: RoundState -> RobotType -> Integer
getRobotCount (RoundState {robotCounter}) robotType =
  getCount robotType robotCounter

getResourceCount :: RoundState -> Resource -> Integer
getResourceCount (RoundState {resourceCounter}) resource =
  getCount resource resourceCounter

getFinalGeodeCount :: RoundState -> Integer
getFinalGeodeCount roundState =
  let currGeodeCount = getResourceCount roundState Geode
      numGeodeRobots = getRobotCount roundState GeodeCracker
      numMinutesLeft = minutesLeft roundState
   in currGeodeCount + (numGeodeRobots * numMinutesLeft)

maxPossibleGeodes :: RoundState -> Integer
maxPossibleGeodes roundState =
  let finalCount = getFinalGeodeCount roundState
      numMinutesLeft = minutesLeft roundState
   in finalCount + sumToN (numMinutesLeft - 1)

-------------------------
-- SECTION: Core logic --
-------------------------
part1 :: [Blueprint] -> Integer
part1 = sum . map (getQualityLevel numMinutes)
  where
    numMinutes = 24

part2 :: [Blueprint] -> Integer
part2 = product . map (getMaxGeodes numMinutes) . take 3
  where
    numMinutes = 32
     
getQualityLevel :: Integer -> Blueprint -> Integer
getQualityLevel numMinutes blueprint@(Blueprint {blueprintId}) =
  blueprintId * (getMaxGeodes numMinutes blueprint)
      
getMaxGeodes :: Integer -> Blueprint -> Integer
getMaxGeodes numMinutes blueprint =
  maximumBound 0 $
    map
      (getMaxGeodes' blueprint (initialRoundState numMinutes) 0)
      allRobotTypes
  
getMaxGeodes' :: Blueprint -> RoundState -> Integer -> RobotType -> Integer
getMaxGeodes' blueprint roundState currMax nextRobotType
  | maxPossibleGeodes roundState < currMax = currMax
  | not canBuild = finalGeodeCount
  | minutesLeft' <= 0 = finalGeodeCount
  | nextRobotType /= GeodeCracker && currRobotCount >= maxRobotCount = finalGeodeCount
  | otherwise = foldr
                  (flip $ getMaxGeodes' blueprint roundState')
                  currMax
                  allRobotTypes
  where
    canBuild = canBuildRobotLater blueprint nextRobotType roundState
    finalGeodeCount = max currMax $ getFinalGeodeCount roundState
    currRobotCount = getRobotCount roundState nextRobotType
    maxRobotCount = getMaxRobotCount blueprint nextRobotType
    roundState' = buildRobot blueprint nextRobotType
                . harvestResourcesForRobot blueprint nextRobotType
                $ roundState
    minutesLeft' = minutesLeft roundState'

canBuildRobotLater :: Blueprint -> RobotType -> RoundState -> Bool
canBuildRobotLater blueprint robotType roundState =
  let requiredRobotTypes = map
                             robotTypeFor
                             $ requiredResources blueprint robotType
      canGatherResources = all
                             ((> 0) <$> getRobotCount roundState)
                             requiredRobotTypes
   in canGatherResources

buildRobot :: Blueprint -> RobotType -> RoundState -> RoundState
buildRobot blueprint robotType roundState =
  let roundState' = harvestResources 1 roundState
      resourceCounter'' = foldr f (resourceCounter roundState') allResources
      f resource counter =
        let cost = getCost blueprint robotType resource
         in updateCount (subtract cost) resource counter 
      robotCounter'' = updateCount (+1) robotType $ robotCounter roundState'
   in roundState'
        { resourceCounter = resourceCounter'',
          robotCounter = robotCounter'' }

harvestResourcesForRobot :: Blueprint -> RobotType -> RoundState -> RoundState
harvestResourcesForRobot blueprint robotType roundState =
  let minutesNeeded = minutesNeededForRobot blueprint robotType roundState
   in harvestResources minutesNeeded roundState

minutesNeededForRobot :: Blueprint -> RobotType -> RoundState -> Integer
minutesNeededForRobot blueprint robotType roundState =
  maximumBound
    0
    (map f $ requiredResources blueprint robotType)
  where
    f resource =
      let requiredRobotType = robotTypeFor resource
          requiredCount = getCost blueprint robotType resource
          currentCount = getResourceCount roundState resource
          numRobots = getRobotCount roundState requiredRobotType
       in if (requiredCount - currentCount) <= 0
            then 0
            else (requiredCount - currentCount) `divRoundUp` numRobots

harvestResources :: Integer -> RoundState -> RoundState
harvestResources numMinutes roundState@(RoundState {minutesLeft, resourceCounter}) =
  roundState
    { minutesLeft = minutesLeft - numMinutes,
      resourceCounter = resourceCounter'
    }
  where
    resourceCounter' = foldr f resourceCounter allRobotTypes
    f robotType resourceCounter'' =
      let numRobots = getRobotCount roundState robotType
          increase = numRobots * numMinutes
          resource = resourceFor robotType
       in updateCount (+ increase) resource resourceCounter''

---------------------------------
-- SECTION: Arithmetic Helpers --
---------------------------------
divRoundUp :: Integral a => a -> a -> a
divRoundUp n d =
  let (quotient, remainder) = n `divMod` d
   in if remainder > 0
        then quotient + 1
        else quotient

sumToN :: Integral a => a -> a
sumToN n
  | n <= 0 = 0
  | otherwise = (n * (n+1)) `div` 2
