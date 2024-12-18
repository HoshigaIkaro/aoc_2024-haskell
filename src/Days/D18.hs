module Days.D18 (run, part1, part2) where

import Control.Arrow
import Data.Char
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S

run :: IO ()
run = do
    input <- readFile "input/d18.txt"
    print $ part1 input
    print $ part2 input

type Point = (Int, Int)

pInput :: String -> Map Point Int
pInput = M.fromList . map f . zip [1 ..] . lines
  where
    f (time, s) =
        let x = takeWhile isDigit s
            y = drop (length x + 1) s
         in ((read y, read x), time)

initialMap :: Point -> Map Point (Maybe Int)
initialMap (height, width) = M.fromList $ map (,Nothing) $ concatMap f [0 .. height - 1]
  where
    f r = [(r, c) | c <- [0 .. width - 1]]

adjacentPoints :: Point -> [Point]
adjacentPoints (r, c) = [(r - 1, c), (r, c - 1), (r, c + 1), (r + 1, c)]

findWaySimp :: Point -> Map Point (Maybe Int) -> [Point]
findWaySimp (height, width) mapping = go [initial] S.empty
  where
    initial = [(0, 0)]
    target = (height - 1, width - 1)
    go :: [[Point]] -> Set Point -> [Point]
    go [] _ = []
    go (stateL : ps) visited
        | point `S.member` visited = go ps visited
        | point == target = stateL
        | otherwise = go (ps <> validAdjacent) newVisited
      where
        point = head stateL
        newVisited = S.insert point visited
        adjacent = adjacentPoints point
        f p = p `M.member` mapping
        validAdjacent = map ((: stateL)) $ filter f adjacent

findWay :: Point -> Map Point (Maybe Int) -> Maybe Int -> [Point]
findWay (height, width) mapping overideTime = go [initial] S.empty
  where
    initial = ([(0, 0)], 0)
    target = (height - 1, width - 1)
    -- target = (6, 5)
    -- target = (height, width)
    go :: [([Point], Int)] -> Set ([Point], Int) -> [Point]
    go [] _ = []
    go (state@(pL, time) : ps) visited
        | state `S.member` visited = go ps visited
        | point == target = pL
        | otherwise = go (ps <> validAdjacent) newVisited
      where
        point = head pL
        newVisited = S.insert state visited
        adjacent = map (,succ time) $ adjacentPoints point
        f (p, t) = case M.lookup p mapping of
            Nothing -> False
            Just mFallTime -> case mFallTime of
                Nothing -> True
                Just fallTime -> case overideTime of
                    Nothing -> t < fallTime
                    Just oT -> t < fallTime || fallTime >= oT
        validAdjacent = map (first (: pL)) $ filter f adjacent

applyTime :: Int -> Map Point (Maybe Int) -> Map Point (Maybe Int)
applyTime time mapping = M.filter f mapping
  where
    f Nothing = True
    f (Just mT) = mT > time

-- part1 :: String -> Int
-- part1 s = findWay size combinedMap (Just 11)
part1 s = pred $ length $ findWaySimp size $ applyTime 1024 combinedMap
  where
    size = (71, 71)
    -- size = (7, 7)
    fallMap = pInput s
    combinedMap = M.map Just fallMap `M.union` initialMap size

findFirstBlocking :: Point -> Map Point (Maybe Int) -> Point
findFirstBlocking size mapping = go 1
  where
    go time
        | null $ findWaySimp size appliedMapping = head $ M.keys $ M.filter ((== time) . fromMaybe 0) mapping
        | otherwise = go $ succ time
      where
        appliedMapping = applyTime time mapping

part2 :: String -> Point
part2 s = ((,) <$> snd <*> fst) $ findFirstBlocking size combinedMap
    where
        size = (71, 71)
        -- size = (7, 7)
        fallMap = pInput s
        combinedMap = M.map Just fallMap `M.union` initialMap size