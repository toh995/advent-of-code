module Day10.Main where

import Control.Category ((>>>))
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (sort, tails)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace
import Text.Parsec (Parsec, between, many, many1, try)
import qualified Text.Parsec.Char as C
import Text.Parsec.String (parseFromFile)

filePath :: FilePath
filePath = "src/Day10/data.txt"

main :: IO ()
main = do
    rows <-
        parseFromFile rowsP filePath
            <&> unwrapEither
    let (targetLightss, buttonss, targetJoltagess) = unzip3 rows

    let part1Answer = part1 $ zip targetLightss buttonss
    putStrLn $ "PART1: " ++ show part1Answer
    let part2Answer = part2 $ zip targetJoltagess buttonss
    putStrLn $ "PART2: " ++ show part2Answer

unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Left a) = error $ show a
unwrapEither (Right b) = b

part1 :: [(LightState, [Button])] -> Int
part1 =
    map (fromJust . uncurry fewestButtonPresses1)
        >>> sum

part2 :: [(JoltageState, [Button])] -> Int
part2 =
    map (fromJust . uncurry fewestButtonPresses2)
        >>> sum

type LightState = IntSet
type JoltageState = IntMap Int

newtype Button = Button {indexes :: [Int]}
    deriving (Eq, Ord, Show)

fewestButtonPresses1 :: LightState -> [Button] -> Maybe Int
fewestButtonPresses1 targetLights allButtons =
    go $
        Seq.fromList [(startLights, allButtons, 0)]
  where
    startLights = IS.empty
    go Empty = Nothing
    go ((currLights, availableButtons, pressCount) :<| queue)
        | currLights == targetLights = Just pressCount
        | otherwise =
            go $
                queue
                    >< Seq.fromList
                        [ (nextLights, otherButtons, pressCount + 1)
                        | (button : otherButtons) <- tails availableButtons
                        , let nextLights = pressButton1 button currLights
                        ]

pressButton1 :: Button -> LightState -> LightState
pressButton1 button lights =
    foldr
        ( \i acc ->
            if i `IS.member` acc
                then IS.delete i acc
                else IS.insert i acc
        )
        lights
        (indexes button)

fewestButtonPresses2 :: JoltageState -> [Button] -> Maybe Int
fewestButtonPresses2 targetJoltages allButtons =
    go $
        Seq.fromList [(startJoltages, allButtons, 0)]
  where
    startJoltages = IM.map (const 0) targetJoltages
    go Empty = Nothing
    go ((currJoltages, availableButtons, pressCount) :<| queue)
        | currJoltages == targetJoltages = Just pressCount
        | currJoltages > targetJoltages = go queue
        | otherwise =
            go $
                queue
                    >< Seq.fromList
                        [ (nextJoltages, nextAvailableButtons, pressCount + 1)
                        | nextAvailableButtons@(button : _) <- tails availableButtons
                        , let nextJoltages = pressButton2 button currJoltages
                        ]

pressButton2 :: Button -> JoltageState -> JoltageState
pressButton2 button joltages =
    foldr
        (IM.adjust (+ 1))
        joltages
        (indexes button)

type Parser = Parsec String ()

rowsP :: Parser [(LightState, [Button], JoltageState)]
rowsP = sepBy1 rowP C.newline

rowP :: Parser (LightState, [Button], JoltageState)
rowP = do
    targetLights <- targetLightsP
    _ <- C.char ' '
    buttons <- sepBy1 buttonP (C.char ' ')
    _ <- C.char ' '
    targetJoltages <- targetJoltagesP
    pure (targetLights, buttons, targetJoltages)

targetLightsP :: Parser LightState
targetLightsP = do
    _ <- C.char '['
    str <- many1 (try $ C.oneOf "#.")
    _ <- C.char ']'
    pure $
        IS.fromList
            [ i
            | (char, i) <- zip str [0 ..]
            , char == '#'
            ]

targetJoltagesP :: Parser JoltageState
targetJoltagesP = do
    _ <- C.char '{'
    nums <- sepBy1 intP (C.char ',')
    _ <- C.char '}'
    pure
        ( IM.fromList $
            zip [0 ..] nums
        )

buttonP :: Parser Button
buttonP =
    between
        (C.char '(')
        (C.char ')')
        (Button <$> sepBy1 intP (C.char ','))

intP :: Parser Int
intP = read <$> many1 (try C.digit)

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 aP sepP = do
    a <- aP
    as <- many (try $ sepP >> aP)
    pure (a : as)
