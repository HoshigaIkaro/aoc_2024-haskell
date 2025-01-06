{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Days.D9 (run, part1, part2) where

import Control.Arrow
import Data.Char (digitToInt)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as M
import Data.List
import Data.Maybe (fromJust, isNothing)
import Data.Sequence (Seq (..), (<|), (|>))
import Data.Sequence qualified as S
import Data.Text qualified as T

run :: IO ()
run = do
    input <- readFile "input/d9.txt"
    print $ part1 input
    print $ part2 input

pInput :: String -> IntMap Int
pInput = go M.empty 0 0 True
  where
    go :: IntMap Int -> Int -> Int -> Bool -> String -> IntMap Int
    go mapping _ _ _ [] = mapping
    go mapping index currentID isFile (x : xs)
        | isFile = go (mapping `M.union` M.fromList newIndices) (index + num) (succ currentID) False xs
        | otherwise = go mapping (index + num) currentID True xs
      where
        num = digitToInt x
        newIndices = map (,currentID) $ take num [index ..]

tryMoveOne :: IntMap Int -> Int -> Maybe (IntMap Int, Int)
tryMoveOne mapping currentMaxIndex = do
    let indices = [0 .. currentMaxIndex]
    freeSpaceIndex <- find (`M.notMember` mapping) indices
    idToMove <- M.lookup currentMaxIndex mapping
    let removedOld = M.delete currentMaxIndex mapping
        newMapping = M.insert freeSpaceIndex idToMove removedOld
        newMaxIndex = until (`M.member` newMapping) pred currentMaxIndex
    pure (newMapping, newMaxIndex)

pToSeq :: String -> Seq (Maybe Int)
pToSeq = go 0 . takeWhile (/= '\n')
  where
    go _ [] = S.empty
    go currentId [x] = S.replicate (digitToInt x) (Just currentId)
    go currentId (x : y : rest) =
        let fileLength = digitToInt x
            freeSpace = digitToInt y
            idSeq :: Seq (Maybe Int)
            idSeq = S.replicate fileLength (Just currentId)
            freeSeq = S.replicate freeSpace Nothing
         in idSeq <> freeSeq <> go (currentId + 1) rest

moveAllV2S :: Seq (Maybe Int) -> Seq Int
moveAllV2S S.Empty = S.Empty
-- current left end is part of a file
moveAllV2S ((Just v) :<| rest) = v <| moveAllV2S rest
-- current left end is free space
moveAllV2S (Nothing :<| rest) = case rest of
    S.Empty -> S.empty
    restR :|> mValR -> case mValR of
        Just v -> v <| moveAllV2S restR
        Nothing -> moveAllV2S (Nothing <| S.dropWhileR isNothing restR)

moveAll :: IntMap Int -> Int -> IntMap Int
moveAll mapping maxIndex = case tryMoveOne mapping maxIndex of
    Nothing -> mapping
    Just (newMapping, newMaxIndex) -> moveAll newMapping newMaxIndex

calculateCheckSum :: IntMap Int -> Int
calculateCheckSum = sum . map (uncurry (*)) . M.assocs

showIds :: IntMap Int -> String
showIds = concatMap (show . snd) . sort . M.toList

part1 :: String -> Int
part1 = calculateCheckSumVS . moveAllV2S . pToSeq
  where
    calculateCheckSumVS = S.foldrWithIndex f 0
    f idx currentId accumulated = idx * currentId + accumulated

findFittingFreeSpace :: IntMap Int -> Int -> Int -> Maybe [Int]
findFittingFreeSpace = go 0
  where
    go start mapping len maxIndex
        | start > maxIndex = Nothing
        | otherwise = do
            let indices = [start .. maxIndex]
            freeSpaceStart <- find (`M.notMember` mapping) indices
            freeSpaceEnd <- find (`M.member` mapping) (drop (freeSpaceStart - start) indices)
            let freeLen = freeSpaceEnd - freeSpaceStart
            if freeLen >= len
                then Just [freeSpaceStart .. freeSpaceEnd - 1]
                else go freeSpaceEnd mapping len maxIndex

tryMoveId :: IntMap Int -> Int -> Int -> Maybe (IntMap Int)
tryMoveId mapping currentId maxIndex = do
    let indices = M.keys $ M.filter (== currentId) mapping
        len = length indices
    freeSpaceWorks <- findFittingFreeSpace mapping len maxIndex
    let removedOld = foldr M.delete mapping indices
        freeSpaceStart = minimum freeSpaceWorks
        newMapping = foldr (`M.insert` currentId) removedOld [freeSpaceStart .. freeSpaceStart + len - 1]

    pure newMapping

moveAllV2 :: IntMap Int -> IntMap Int
moveAllV2 originalMapping = go (reverse [0 .. maxId]) originalMapping
  where
    maxId = maximum $ M.elems originalMapping
    maxIndexFor mapping x = maximum . map fst $ M.toList $ M.filter (== x) mapping
    go [] mapping = mapping
    go (x : xs) mapping = case tryMoveId mapping x (maxIndexFor mapping x) of
        Nothing -> go xs mapping
        Just newMapping -> go xs newMapping

part2 :: String -> Int
part2 = calculateCheckSum . moveAllV2 . pInput
-- part2 = const 0

pPairs :: String -> [(Int, Int)]
pPairs = map ((f *** f) . T.splitAt 1) . T.chunksOf 2 . T.pack
  where
    f = read . T.unpack