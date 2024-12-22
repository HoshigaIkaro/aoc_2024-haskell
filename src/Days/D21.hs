module Days.D21 (run, part1, part2) where

import Control.Arrow
import Data.Char
import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Tuple

run :: IO ()
run = do
    input <- readFile "input/d21.txt"
    print $ part1 input
    print $ part2 input

type Point = (Int, Int)

numericalKeypadMap :: Map Point Char
numericalKeypadMap =
    M.fromList
        [ ((0, 0), '7')
        , ((0, 1), '8')
        , ((0, 2), '9')
        , ((1, 0), '4')
        , ((1, 1), '5')
        , ((1, 2), '6')
        , ((2, 0), '1')
        , ((2, 1), '2')
        , ((2, 2), '3')
        , ((3, 1), '0')
        , ((3, 2), 'A')
        ]

numericalToPointMap :: Map Char Point
numericalToPointMap = M.fromList $ map swap $ M.toList numericalKeypadMap

directionalMap :: Map Point Char
directionalMap =
    M.fromList
        [ ((0, 1), '^')
        , ((0, 2), 'A')
        , ((1, 0), '<')
        , ((1, 1), 'v')
        , ((1, 2), '>')
        ]

directionalToPointMap :: Map Char Point
directionalToPointMap = M.fromList $ map swap $ M.toList directionalMap

rightAPathsFrom :: Map Point Char -> Point -> Point -> [[Char]]
rightAPathsFrom mapping start@(sR, sC) (tR, tC) = nubOrd $ map (map f . g) (nA <> nB)
  where
    f ((aR, aC), (bR, bC))
        | aR > bR = '^'
        | aR < bR = 'v'
        | aC > bC = '<'
        | otherwise = '>'
    g = liftA2 zip id (drop 1)
    dR = tR - sR
    dC = tC - sC
    aOne = take (1 + abs dR) $ iterate (first (+ signum dR)) start
    aTwo = take (abs dC) $ drop 1 $ iterate (second (+ signum dC)) (last aOne)
    a = aOne <> aTwo
    nA = [a | all (`M.member` mapping) a]
    bOne = take (1 + abs dC) $ iterate (second (+ signum dC)) start
    bTwo = take (abs dR) $ drop 1 $ iterate (first (+ signum dR)) (last bOne)
    b = bOne <> bTwo
    nB = [b | all (`M.member` mapping) b]

nToNMap :: Map Char (Map Char [[Char]])
nToNMap = M.fromList $ go ('A' : ['0' .. '9'])
  where
    go [] = []
    go (x : xs) =
        let start = numericalToPointMap M.! x
            f (target, y) = (y, rightAPathsFrom numericalKeypadMap start target)
         in (x, M.fromList $ map f (M.toList numericalKeypadMap)) : go xs

dToDMap :: Map Char (Map Char [[Char]])
dToDMap = M.fromList $ go ['A', '^', 'v', '<', '>']
  where
    go [] = []
    go (x : xs) =
        let start = directionalToPointMap M.! x
            f (target, y) = (y, rightAPathsFrom directionalMap start target)
         in (x, M.fromList $ map f (M.toList directionalMap)) : go xs

type Cache = Map (Char, Char, Int) Int

minCostBetween :: Char -> Char -> Int -> Cache -> Bool -> (Int, Cache)
minCostBetween _ _ 0 cache _ = (1, cache)
minCostBetween a b depth cache topMost =
    case M.lookup (a, b, depth) cache of
        Just val -> (val, cache)
        Nothing -> updateCache $ outerGo possibleWays cache
  where
    basePossible
        | topMost = nToNMap M.! a M.! b
        | otherwise = dToDMap M.! a M.! b
    f p = liftA2 zip id (drop 1) ('A' : p <> "A")
    possibleWays = map f basePossible
    updateCache (v, wCache) = (v, M.insert (a, b, depth) v wCache)
    outerGo :: [[(Char, Char)]] -> Cache -> (Int, Cache)
    outerGo [] vCache = (maxBound :: Int, vCache)
    outerGo (x : xs) vCache = let (res, wCache) = innerGo x vCache in first (min res) $ outerGo xs wCache
    innerGo :: [(Char, Char)] -> Cache -> (Int, Cache)
    innerGo [] vCache = (0, vCache)
    innerGo ((x, y) : zs) vCache =
        let (cost, wCache) = minCostBetween x y (depth - 1) vCache False
         in first (+ cost) $ innerGo zs wCache

complexityV2 :: (String, Int) -> Int
complexityV2 (code, p) = n * p
  where
    n = read $ filter isDigit code

computeAnswer :: Int -> String -> Int
computeAnswer numRobotArms = sum . map complexityV2 . fst . processAll . lines
  where
    prepare code = zip ('A' : code) code
    process code (results, cache) =
        first ((: results) . (code,))
            $ foldr
                ( \(a, b) (soFar, vCache) ->
                    let (res, newCache) = minCostBetween a b numRobotArms vCache True
                     in (soFar + res, newCache)
                )
                (0, cache)
            $ prepare code
    processAll = foldr process ([], mempty)

part1 :: String -> Int
part1 = computeAnswer 3

part2 :: String -> Int
part2 = computeAnswer 26
