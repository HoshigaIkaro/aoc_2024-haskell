module Days.D22 (run, part1, part2) where

import Control.Concurrent (setNumCapabilities)
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (parListChunk, parMap, rdeepseq, rpar, using)
import Data.Bits
import Data.Containers.ListUtils (nubOrdOn)
import Data.Either (rights)
import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as M
import Data.List (zip5)
import Data.Text qualified as T
import Data.Text.Read qualified as T

run :: IO ()
run = do
    setNumCapabilities 32
    input <- readFile "input/d22.txt"
    print $ part1 input
    print $ part2 input

mix :: Int -> Int -> Int
mix = xor

prune :: Int -> Int
prune = (.&. ((1 .<<. 24) - 1))

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

pInput :: String -> [Int]
pInput = map fst . rights . map T.decimal . T.lines . T.pack

part1 :: String -> Int
part1 = sum . parMap rpar (nthNewSecretFrom 2000) . pInput

windowsForNSecretNumbers :: Int -> Int -> [(Int, Int, Int, Int, Int)]
windowsForNSecretNumbers n = f . map (`mod` 10) . take n . drop 1 . iterate nextSecret
  where
    f = zip5 <$> id <*> drop 1 <*> drop 2 <*> drop 3 <*> drop 4

toKey :: (Int, Int, Int, Int) -> Int
toKey (a, b, c, d) = (toChunk a .<<. 24) .|. (toChunk b .<<. 16) .|. (toChunk c .<<. 8) .|. toChunk d
  where
    toChunk n =
        let gamma = if signum n >= 0 then 0 else 1
         in (gamma .<<. 7) .|. abs n

prepareSequencesForMap :: [(Int, Int, Int, Int, Int)] -> [(Int, Int)]
prepareSequencesForMap = foldr f []
  where
    f (a, b, c, d, e) =
        let key = toKey (b - a, c - b, d - c, e - d)
         in ((key, e) :)

-- createMapping :: [((Int, Int, Int, Int), Int)] -> Map (Int, Int, Int, Int) Int
-- createMapping = foldr f M.empty . reverse
--   where
--     f (key, value) = M.alter (Just . fromMaybe value) key

createMapping :: [(Int, Int)] -> IntMap Int
createMapping = M.fromList . nubOrdOn fst

-- combineP :: [IntMap Int] -> IntMap Int
-- combineP [] = M.empty
-- combineP [x] = x
-- combineP lst = p `par` q `pseq` M.unionWith (+) p q
--   where
--     len = length lst
--     mid = len `div` 2
--     p = combineP (take mid lst)
--     q = combineP (drop mid lst)

part2 :: String -> Int
part2 = maximum . M.elems . f . flip using (parListChunk 50 rdeepseq) . map process . pInput
  where
    numSecrets = 2001
    process = createMapping . prepareSequencesForMap . windowsForNSecretNumbers numSecrets
    f = foldr (M.unionWith (+)) M.empty