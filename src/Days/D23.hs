{-# LANGUAGE OverloadedStrings #-}

module Days.D23 (run, part1, part2) where

import Control.Monad
import Data.Char (isAlpha, isDigit)
import Data.Containers.ListUtils (nubOrdOn)
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

run :: IO ()
run = do
    input <- readFile "input/d23.txt"
    print $ part1 input
    print $ part2 input

type Parser = Parsec Void Text

pPair :: Parser (Text, Text)
pPair = do
    a <- takeWhile1P Nothing isAlpha
    void $ char '-'
    b <- takeWhile1P Nothing isAlpha
    pure (a, b)

pInputToList :: String -> [(Text, Text)]
pInputToList = fromJust . parseMaybe (sepEndBy pPair eol) . T.pack

toGraph :: [(Text, Text)] -> Map Text [Text]
toGraph = foldr f M.empty
  where
    f (a, b) = M.alter (Just . maybe [a] (a :)) b . M.alter (Just . maybe [b] (b :)) a

findCompleteThreeFor :: Map Text [Text] -> Text -> [(Text, Text, Text)]
findCompleteThreeFor mapping a = nubOrdOn (\(x, y, z) -> S.fromList [x, y, z]) $ go (mapping M.! a) (S.singleton a)
  where
    go [] _ = []
    go (b : bs) visited =
        let cs = [c | c <- mapping M.! b, c `elem` mapping M.! a]
         in [(a, b, c) | c <- cs] <> go bs visited

part1 :: String -> Int
part1 s = length $ nubOrdOn (\(a, b, c) -> S.fromList [a, b, c]) $ filter atLeastOneT $ concatMap (findCompleteThreeFor g) nodes
  where
    g = toGraph $ pInputToList s
    nodes = M.keys g
    atLeastOneT (a, b, c) = any ("t" `T.isPrefixOf`) [a, b, c]

allConnectedStartingWith :: Map Text [Text] -> Text -> Set Text
allConnectedStartingWith mapping start = go [start] S.empty S.empty
  where
    go [] result _ = result
    go (x : xs) result visited
        | x `S.member` visited = go xs result visited
        | all (connected x) (S.toList result) = go (xs <> possible) (S.insert x result) newVisited
        | otherwise = go xs result visited
      where
        connected a b = b `elem` mapping M.! a
        newVisited = S.insert x visited
        possible = filter (`S.notMember` newVisited) $ mapping M.! x

part2 :: String -> Text
part2 s = T.intercalate "," $ sort $ S.toList $ maximumBy largestSize $ map (allConnectedStartingWith g) nodes
  where
    g = toGraph $ pInputToList s
    nodes = M.keys g
    largestSize a b = compare (S.size a) (S.size b)