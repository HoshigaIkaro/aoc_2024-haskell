module Days.D22 (run, part1, part2) where

import Control.Concurrent (setNumCapabilities)
import Control.Parallel ( par, pseq )
import Control.Parallel.Strategies (parMap, rpar)
import Data.Bits
import Data.List (zip5)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe ( fromMaybe )

run :: IO ()
run = do
    setNumCapabilities 32
    input <- readFile "input/d22.txt"
    print $ part1 input
    print $ part2 input

mix :: Int -> Int -> Int
mix secret value = secret `xor` value

prune :: Int -> Int
prune secret = secret .&. ((1 .<<. 24) - 1)

stepOne :: Int -> Int
stepOne = prune . liftA2 mix id (.<<. 6)

stepTwo :: Int -> Int
stepTwo = prune . liftA2 mix id (.>>. 5)

stepThree :: Int -> Int
stepThree = prune . liftA2 mix id (.<<. 11)

nextSecret :: Int -> Int
nextSecret = stepThree . stepTwo . stepOne

nthNewSecretFrom :: Int -> Int -> Int
nthNewSecretFrom 0 = id
nthNewSecretFrom n = nthNewSecretFrom (n - 1) . nextSecret

part1 :: String -> Int
part1 = sum . parMap rpar (nthNewSecretFrom 2000 . read) . lines

windowsForNSecretNumbers :: Int -> Int -> [(Int, Int, Int, Int, Int)]
windowsForNSecretNumbers n = f . map (`mod` 10) . take n . drop 1 . iterate nextSecret
  where
    f = zip5 <$> id <*> drop 1 <*> drop 2 <*> drop 3 <*> drop 4

prepareSequencesForMap :: [(Int, Int, Int, Int, Int)] -> [((Int, Int, Int, Int), Int)]
prepareSequencesForMap = foldr f []
  where
    f (a, b, c, d, e) = (((b - a, c - b, d - c, e - d), e) :)

createMapping :: [((Int, Int, Int, Int), Int)] -> Map (Int, Int, Int, Int) Int
createMapping = foldr f M.empty . reverse
  where
    f (key, value) = M.alter (Just . fromMaybe value) key

combineP :: [Map (Int, Int, Int, Int) Int] -> Map (Int, Int, Int, Int) Int
combineP [] = M.empty
combineP [x] = x
combineP lst = p `par` q `pseq` M.unionWith (+) p q
  where
    len = length lst
    mid = len `div` 2
    p = combineP (take mid lst)
    q = combineP (drop mid lst)

part2 :: String -> Int
part2 = maximum . M.elems . combineP . map process . lines
  where
    numSecrets = 2001
    process = createMapping . prepareSequencesForMap . windowsForNSecretNumbers numSecrets . read