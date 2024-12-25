{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.D25 (run, part1, part2) where

import Data.List
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (eol)

run :: IO ()
run = do
    input <- readFile "input/d25.txt"
    print $ part1 input
    print $ part2 input

data Object = Lock [Int] | Key [Int] deriving (Show, Eq, Ord)

type Parser = Parsec Void Text

getHeights :: [Text] -> [Int]
getHeights = map (length . filter (== '#')) . transpose . map T.unpack

pObject :: Parser Object
pObject = do
    lns <- sepEndBy1 (takeWhile1P Nothing (`elem` ".#")) eol
    let obj =
            if T.all (== '#') $ head lns
                then Lock
                else Key
    pure $ obj $ getHeights lns

pInput :: String -> [Object]
pInput = fromJust . parseMaybe (sepEndBy1 pObject eol) . T.pack

separateObjects :: [Object] -> ([Object], [Object])
separateObjects = partition f
  where
    f (Lock _) = True
    f _ = False

nonOverlaps :: Object -> Object -> Bool
nonOverlaps obj1 obj2 = case (obj1, obj2) of
    (Lock nums1, Key nums2) -> all f $ zip nums1 nums2
    _ -> False
  where
    f (a, b) = a + b <= 7

numKeysFitLock :: Object -> [Object] -> Int
numKeysFitLock lock = length . filter (nonOverlaps lock)

part1 :: String -> Int
part1 s = sum $ map (`numKeysFitLock` keys) locks
  where
    (locks, keys) = separateObjects $ pInput s

part2 :: String -> Int
part2 _ = 0