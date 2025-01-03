module Days.D2 (run, part1, part2) where

import Data.Either (rights)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import GHC.Arr (inRange)

run :: IO ()
run = do
    input <- readFile "input/d2.txt"
    print $ part1 input
    print $ part2 input

adjPair :: [Int] -> [(Int, Int)]
adjPair = zip <*> drop 1

isSafe :: [Int] -> Bool
isSafe a = (allDecreasing pairs || allIncreasing pairs) && adjacentDiffValid pairs
  where
    pairs = adjPair a
    allIncreasing = all (uncurry (<))
    allDecreasing = all (uncurry (>))
    adjacentDiffValid = all (inRange (1, 3) . abs . uncurry (-))

numSafeWith :: ([Int] -> Bool) -> String -> Int
numSafeWith func = length . filter id . map applyFuncOnLine . T.lines . T.pack
  where
    toInts = map fst . rights . map T.decimal . T.words
    applyFuncOnLine = func . toInts

part1 :: String -> Int
part1 = numSafeWith isSafe

fixable :: [Int] -> Bool
fixable xs = or [isSafe $ removed n | n <- [0 .. length xs - 1]]
  where
    removed n = take n xs <> drop (succ n) xs

part2 :: String -> Int
part2 = numSafeWith $ liftA2 (||) isSafe fixable
