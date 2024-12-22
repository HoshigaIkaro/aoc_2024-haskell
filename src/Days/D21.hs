module Days.D21 (run, part1, part2) where

import Control.Arrow
import Data.Char
import Data.Containers.ListUtils (nubOrd)
import Data.Heap qualified as H
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
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

adjacentPointDirs :: Point -> [(Point, Char)]
adjacentPointDirs (r, c) = [((r - 1, c), '^'), ((r + 1, c), 'v'), ((r, c - 1), '<'), ((r, c + 1), '>')]

pathFromTo' :: Map Point Char -> Point -> Point -> [Char]
pathFromTo' mapping start target = reverse $ go [(start, [])] S.empty
  where
    go [] _ = []
    go ((point, pathSoFar) : xs) visited
        | point == target = pathSoFar
        | point `S.member` visited = go xs visited
        | otherwise = go (xs <> newStates) newVisited
      where
        newVisited = S.insert point visited
        validAdjacent = filter ((`M.member` mapping) . fst) $ adjacentPointDirs point
        newStates = map (second (: pathSoFar)) validAdjacent

pathFromTo :: Map Point Char -> Point -> Point -> [Char]
pathFromTo mapping start target = reverse $ go initialHeap S.empty
  where
    initialHeap = H.singleton $ H.Entry 0 (start, [])
    go heap visited
        | H.null heap = []
        | point == target = pathSoFar
        | point `S.member` visited = go newHeap visited
        | otherwise = go (heap <> additionalHeap) newVisited
      where
        minState = H.minimum heap
        (point, pathSoFar) = H.payload minState
        newHeap = H.deleteMin heap
        newVisited = (S.insert point visited)
        validAdjacent = filter ((`M.member` mapping) . fst) $ adjacentPointDirs point
        newStates = map (second (: pathSoFar)) validAdjacent
        additionalHeap = H.fromList $ map (H.Entry <$> (length . snd) <*> id) newStates

numericalCodeToPoints :: [Char] -> [Point]
numericalCodeToPoints = map (numericalToPointMap M.!)

startEndPairs :: [Point] -> [(Point, Point)]
startEndPairs = liftA2 zip id (drop 1)

codeToPointPairs :: [Char] -> [(Point, Point)]
codeToPointPairs = startEndPairs . numericalCodeToPoints . ('A' :)

nToNMap :: Map Char (Map Char [Char])
nToNMap = M.fromList $ go ('A' : ['0' .. '9'])
  where
    go [] = []
    go (x : xs) =
        let start = numericalToPointMap M.! x
            f (target, y) = (y, pathFromTo' numericalKeypadMap start target)
         in (x, M.fromList $ map f (M.toList numericalKeypadMap)) : go xs

dToDMap :: Map Char (Map Char [Char])
dToDMap = M.fromList $ go (['A', '^', 'v', '<', '>'])
  where
    go [] = []
    go (x : xs) =
        let start = directionalToPointMap M.! x
            f (target, y) = (y, pathFromTo' directionalMap start target)
         in (x, M.fromList $ map f (M.toList directionalMap)) : go xs

codeToPairs :: [Char] -> [(Char, Char)]
codeToPairs code =
    let newCode = 'A' : code
     in zip newCode (drop 1 newCode)

complexity :: (String, [Char]) -> Int
complexity (code, p) = n * length p
  where
    n = read $ filter isDigit code

-- allPathsFrom :: Map Point Char -> Point -> Point -> [[Char]]
-- allPathsFrom mapping start target = go [(start, [])] S.empty Nothing
--   where
--     go [] _ _ = []
--     go ((point, pathSoFar) : xs) visited minLength
--         | point == target =
--             case minLength of
--                 Nothing -> reverse pathSoFar : go xs visited (Just $ length pathSoFar)
--                 Just len ->
--                     if length pathSoFar > len
--                         then go xs visited minLength
--                         else reverse pathSoFar : go xs visited minLength
--         | pathSoFar `S.member` visited = go xs visited minLength
--         | otherwise =
--             case minLength of
--                 Nothing -> go (xs <> newStates) newVisited minLength
--                 Just len ->
--                     if length pathSoFar > len
--                         then []
--                         else go (xs <> newStates) newVisited minLength
--       where
--         newVisited = S.insert pathSoFar visited
--         validAdjacent = filter ((`M.member` mapping) . fst) $ adjacentPointDirs point
--         newStates = map (second (: pathSoFar)) validAdjacent

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
    nA = if all (`M.member` mapping) a then [a] else []
    bOne = take (1 + abs dC) $ iterate (second (+ signum dC)) start
    bTwo = take (abs dR) $ drop 1 $ iterate (first (+ signum dR)) (last bOne)
    b = bOne <> bTwo
    nB = if all (`M.member` mapping) b then [b] else []

rightAPathsFrom' :: Map Point Char -> Point -> Point -> [[Char]]
rightAPathsFrom' = ((map (<> "A") .) .) . rightAPathsFrom

nToNMap' :: Map Char (Map Char [[Char]])
nToNMap' = M.fromList $ go ('A' : ['0' .. '9'])
  where
    go [] = []
    go (x : xs) =
        let start = numericalToPointMap M.! x
            f (target, y) = (y, rightAPathsFrom numericalKeypadMap start target)
         in (x, M.fromList $ map f (M.toList numericalKeypadMap)) : go xs

dToDMap' :: Map Char (Map Char [[Char]])
dToDMap' = M.fromList $ go (['A', '^', 'v', '<', '>'])
  where
    go [] = []
    go (x : xs) =
        let start = directionalToPointMap M.! x
            f (target, y) = (y, rightAPathsFrom directionalMap start target)
         in (x, M.fromList $ map f (M.toList directionalMap)) : go xs

combinedMap :: Map Char (Map Char [[Char]])
combinedMap = nToNMap' `M.union` dToDMap'

type Cache = Map (Char, Char, Int) Int

minCostBetween :: Char -> Char -> Int -> Cache -> Bool -> (Int, Cache)
minCostBetween _ _ 0 cache _ = (1, cache)
minCostBetween a b depth cache topMost =
    case M.lookup (a, b, depth) cache of
        Just val -> (val, cache)
        Nothing -> updateCache $ outerGo possibleWays cache
  where
    basePossible
        | topMost = nToNMap' M.! a M.! b
        | otherwise = dToDMap' M.! a M.! b
    f p = liftA2 zip id (drop 1) $ ('A' : p <> "A")
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

-- part1 :: String -> Int
-- part1 s = combinedMap M.! 'A'
part1 = sum .  map complexityV2 . fst. processAll . lines
  where
    depth = 3
    prepare code = zip ('A' : code) code
    process code (results, cache) =
        first ((: results) . (code,))
            $ foldr
                ( \(a, b) (soFar, vCache) ->
                    let (res, newCache) = minCostBetween a b depth vCache True
                     in (soFar + res, newCache)
                )
                (0, cache)
            $ prepare code
    processAll = foldr process ([], mempty)

part2 :: String -> Int
part2 = sum .  map complexityV2 . fst. processAll . lines
  where
    depth = 26
    prepare code = zip ('A' : code) code
    process code (results, cache) =
        first ((: results) . (code,))
            $ foldr
                ( \(a, b) (soFar, vCache) ->
                    let (res, newCache) = minCostBetween a b depth vCache True
                     in (soFar + res, newCache)
                )
                (0, cache)
            $ prepare code
    processAll = foldr process ([], mempty)