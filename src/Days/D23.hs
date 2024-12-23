{-# LANGUAGE OverloadedStrings #-}

module Days.D23 (run, part1, part2) where

import Control.Monad (void)
import Data.Char (isAlpha)
import Data.Containers.ListUtils (nubOrdOn)
import Data.List (maximumBy, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec (
    MonadParsec (takeWhile1P),
    Parsec,
    parseMaybe,
    sepEndBy,
 )
import Text.Megaparsec.Char (char, eol)

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

nubByNodeSet :: [(Text, Text, Text)] -> [(Text, Text, Text)]
nubByNodeSet = nubOrdOn (\(x, y, z) -> S.fromList [x, y, z])

findCompleteThreeFor :: Map Text [Text] -> Text -> [(Text, Text, Text)]
findCompleteThreeFor mapping a = go (mapping M.! a)
  where
    go [] = []
    go (b : bs)  =
        let cs = [c | c <- mapping M.! b, c `elem` mapping M.! a]
         in [(a, b, c) | c <- cs] <> go bs

part1 :: String -> Int
part1 s =
    length $
        nubByNodeSet $
            concatMap (filter atLeastOneStartsWithT . findCompleteThreeFor g) nodes
  where
    g = toGraph $ pInputToList s
    nodes = M.keys g
    atLeastOneStartsWithT (a, b, c) = any ("t" `T.isPrefixOf`) [a, b, c]

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
part2 s =
    T.intercalate "," $
        sort $
            S.toList $
                maximumBy largerSize $
                    map (allConnectedStartingWith g) nodes
  where
    g = toGraph $ pInputToList s
    nodes = M.keys g
    largerSize a b = compare (S.size a) (S.size b)