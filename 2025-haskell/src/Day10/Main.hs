{-# LANGUAGE NamedFieldPuns #-}

module Day10.Main where

import Control.Category ((>>>))
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import Data.Foldable (find, toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Ix (Ix)
import Data.List (tails)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Ratio (Ratio, denominator, numerator)
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
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
    map (uncurry fewestButtonPresses2)
        >>> sum

type LightState = IntSet
type JoltageState = Array I Int

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

data LinearSystem = LinearSystem
    { target :: IntMap (Ratio Int)
    , coefficients :: Map (I, J) (Ratio Int)
    , iMax :: Int
    , jMax :: Int
    }

type I = Int
type J = Int

fewestButtonPresses2 :: JoltageState -> [Button] -> Int
fewestButtonPresses2 targetJoltages buttons =
    minimum
        [ sum $ freeVarVals ++ pivotVals'
        | freeVarVals <- freeVarValss
        , let pivotValsM =
                pivotVals
                    (zip freeVarIndexes' freeVarVals)
                    finalLinearSystem
                    & mapM ratioToIntegral
        , (Just pivotVals') <- [pivotValsM]
        , all (>= 0) pivotVals'
        ]
  where
    finalLinearSystem =
        simplify $
            newLinearSystem targetJoltages buttonsArr
    freeVarIndexes' = freeVarIndexes finalLinearSystem
    freeVarValss =
        cartesian $
            map
                boundsForButtonIndex
                freeVarIndexes'
    boundsForButtonIndex j = [0 .. upperBound]
      where
        button = buttonsArr A.! j
        upperBound = minimum [targetJoltages A.! i | i <- indexes button]
    buttonsArr = A.listArray (0, length buttons - 1) buttons

cartesian :: [[a]] -> [[a]]
cartesian = foldr (liftA2 (:)) [[]]

ratioToIntegral :: (Integral a) => Ratio a -> Maybe a
ratioToIntegral r
    | denominator r == 1 = Just $ numerator r
    | otherwise = Nothing

arrSize :: (Ix i, Integral i) => Array i a -> i
arrSize =
    A.bounds
        >>> uncurry subtract
        >>> (+ 1)

newLinearSystem :: JoltageState -> Array I Button -> LinearSystem
newLinearSystem targetJoltages buttons =
    LinearSystem
        { target = IM.fromList [(i, fromIntegral val) | (i, val) <- A.assocs targetJoltages]
        , coefficients =
            M.fromList
                [ ((i, j), if i `IS.member` buttonIndexSet then 1 else 0)
                | (j, button) <- zip [0 ..] (toList buttons)
                , let buttonIndexSet = IS.fromList $ indexes button
                , i <- [0 .. iMax]
                ]
        , iMax
        , jMax
        }
  where
    iMax = arrSize targetJoltages - 1
    jMax = arrSize buttons - 1

simplify :: LinearSystem -> LinearSystem
simplify =
    go 0
  where
    go pivotISwapDest linearSystem =
        case minimumMay
            [ (pivotJ', i)
            | i <- [pivotISwapDest .. iMax linearSystem]
            , (Just pivotJ') <- [pivotJ i linearSystem]
            ] of
            Nothing -> linearSystem
            Just (pivotJ', pivotI) ->
                let linearSystem' =
                        linearSystem
                            & flip
                                ( foldr
                                    ( \i l ->
                                        l
                                            & multiplyRow
                                                ( negate $
                                                    coefficientAt (i, pivotJ') l
                                                        / coefficientAt (pivotI, pivotJ') l
                                                )
                                                pivotI
                                            & addRows i pivotI
                                    )
                                )
                                [ i
                                | i <- [0 .. iMax linearSystem]
                                , i /= pivotI
                                , coefficientAt (i, pivotJ') linearSystem /= 0
                                ]
                            & ( \l ->
                                    multiplyRow
                                        ( recip $
                                            coefficientAt (pivotI, pivotJ') l
                                        )
                                        pivotI
                                        l
                              )
                            & swapRow pivotI pivotISwapDest
                 in go (pivotISwapDest + 1) linearSystem'

minimumMay :: (Ord a) => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay xs@(_ : _) = Just $ minimum xs

coefficientAt :: (I, J) -> LinearSystem -> Ratio Int
coefficientAt (i, j) LinearSystem{coefficients} = coefficients M.! (i, j)

swapRow :: I -> I -> LinearSystem -> LinearSystem
swapRow i1 i2 l@LinearSystem{target, coefficients, jMax} =
    l
        { target =
            target
                & IM.insert i1 (target IM.! i2)
                & IM.insert i2 (target IM.! i1)
        , coefficients =
            coefficients
                & flip
                    (foldr (\j -> M.insert (i1, j) (coefficients M.! (i2, j))))
                    [0 .. jMax]
                & flip
                    (foldr (\j -> M.insert (i2, j) (coefficients M.! (i1, j))))
                    [0 .. jMax]
        }

multiplyRow :: Ratio Int -> I -> LinearSystem -> LinearSystem
multiplyRow c i l@LinearSystem{target, coefficients, jMax} =
    l
        { target = IM.adjust (* c) i target
        , coefficients =
            foldr
                (\j -> M.adjust (* c) (i, j))
                coefficients
                [0 .. jMax]
        }

addRows :: I -> I -> LinearSystem -> LinearSystem
addRows i1 i2 l@LinearSystem{target, coefficients, jMax} =
    l
        { target = IM.adjust ((+) $ target IM.! i2) i1 target
        , coefficients =
            foldr
                ( \j ->
                    M.adjust
                        ((+) $ coefficients M.! (i2, j))
                        (i1, j)
                )
                coefficients
                [0 .. jMax]
        }

freeVarIndexes :: LinearSystem -> [J]
freeVarIndexes l@LinearSystem{jMax} =
    filter
        (`IS.notMember` pivotVarSet)
        [0 .. jMax]
  where
    pivotVarSet = IS.fromList $ allPivotJs l

pivotJ :: I -> LinearSystem -> Maybe J
pivotJ i linearSystem =
    find
        (\j -> coefficientAt (i, j) linearSystem /= 0)
        [0 .. jMax linearSystem]

allPivotJs :: LinearSystem -> [J]
allPivotJs linearSystem =
    mapMaybe
        (`pivotJ` linearSystem)
        [0 .. iMax linearSystem]

pivotVals :: [(J, Int)] -> LinearSystem -> [Ratio Int]
pivotVals freeVarAssignments l@LinearSystem{coefficients, target, iMax} =
    mapMaybe singleRow [0 .. iMax]
  where
    singleRow i
        | hasPivotVar i =
            Just $
                (target IM.! i)
                    - sum
                        [ fromIntegral val * coefficients M.! (i, j)
                        | (j, val) <- freeVarAssignments
                        ]
        | otherwise = Nothing
    hasPivotVar i = isJust $ pivotJ i l

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
    pure $ A.listArray (0, length nums - 1) nums

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
