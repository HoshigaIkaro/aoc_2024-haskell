{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Days.D24 (run, part1, part2) where

import Control.Arrow
import Control.Monad
import Data.Bits
import Data.Char (isAlphaNum)
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.GraphViz
import Data.GraphViz.Attributes.Colors (
    NamedColor (toColor),
    WeightedColor (WC),
 )
import Data.GraphViz.Attributes.Complete (Attribute (Color, Rank, RankDir), RankDir (FromBottom, FromTop))
import Data.GraphViz.Types.Graph (addDotEdge, addDotNode, emptyGraph, mkGraph, toCanonical)
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import System.Random
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

run :: IO ()
run = do
    input <- readFile "input/d24.txt"
    print $ part1 input
    v <- part2 input
    print v

type Parser = Parsec Void Text

pWireValue :: Parser (Text, Int)
pWireValue = do
    wire <- takeWhile1P (Just "wire label") isAlphaNum
    void $ string ": "
    value <- L.decimal
    pure (wire, value)

pInitialValues :: Parser (Map Text Int)
pInitialValues = M.fromList <$> sepEndBy1 pWireValue eol

data Gate = AND | OR | XOR deriving (Show, Eq)

pConnection :: Parser (Text, (Gate, Text, Text))
pConnection = do
    inputA <- takeWhile1P (Just "left wire") isAlphaNum
    void $ char ' '
    gate <- AND <$ string "AND" <|> OR <$ string "OR" <|> XOR <$ string "XOR"
    void $ char ' '
    inputB <- takeWhile1P (Just "right wire") isAlphaNum
    void $ string " -> "
    output <- takeWhile1P (Just "output wire") isAlphaNum
    pure (output, (gate, inputA, inputB))

pAllConnections :: Parser (Map Text (Gate, Text, Text))
pAllConnections = M.fromList <$> sepEndBy1 pConnection eol

pInput :: String -> (Map Text Int, Map Text (Gate, Text, Text))
pInput = fromJust . parseMaybe go . T.pack
  where
    go = do
        a <- pInitialValues
        void eol
        b <- pAllConnections
        pure (a, b)

compute :: Gate -> Int -> Int -> Int
compute AND = (.&.)
compute OR = (.|.)
compute XOR = (xor)

findValue :: Map Text Int -> Map Text (Gate, Text, Text) -> Text -> (Int, Map Text Int)
findValue mapping outputMap target = case M.lookup target mapping of
    Just v -> (v, mapping)
    Nothing ->
        let (gate, inputA, inputB) = outputMap M.! target
            (outputA, bMapping) = findValue mapping outputMap inputA
            (outputB, cMapping) = findValue bMapping outputMap inputB
            result = compute gate outputA outputB
         in (result, M.insert target result cMapping)

findValues :: Map Text Int -> Map Text (Gate, Text, Text) -> Char -> ([Int], Map Text Int)
findValues valueMap outputMap targetChar = go zWires valueMap
  where
    zWires = sort $ nubOrd $ concatMap (filter (T.singleton targetChar `T.isPrefixOf`)) [M.keys valueMap, M.keys outputMap]
    go [] mapping = ([], mapping)
    go (z : zs) mapping =
        let (val, newMapping) = findValue mapping outputMap z
         in first (val :) $ go zs newMapping

binToInt :: [Int] -> Int
binToInt = sum . zipWith (\a b -> 2 ^ a * b) [0 :: Int ..]

intToBin :: Int -> [Int]
intToBin 0 = []
intToBin n = n `mod` 2 : intToBin (n `div` 2)

part1 :: String -> Int
part1 s = binToInt $ fst $ findValues valueMap outputMap 'z'
  where
    (valueMap, outputMap) = pInput s

cText :: Int -> Char -> Text
cText zNum c = T.pack (c : replicate (2 - length a) '0' <> a)
  where
    a = show zNum


toValueMapFor :: [Int] -> Char -> Map Text Int
toValueMapFor lst c = go $ zip [0 ..] lst
  where
    go [] = mempty
    go ((index, value) : rest) =
        let key = cText index c
         in M.insert key value $ go rest

testMachineFor :: Map Text (Gate, Text, Text) -> Int -> Int -> Bool
testMachineFor machine numOutputBits zNum = go 50 (mkStdGen 1)
  where
    go 0 _ = True
    go n gen1 =
        let (x, gen2) = uniformR (0, (1 .<<. (numOutputBits - 1)) - 1) gen1
            xBin = intToBin x
            (y, gen3) = uniformR (0, (1 .<<. (numOutputBits - 1)) - 1) gen2
            yBin = intToBin y
            target = intToBin (x + y)
            padded bin = bin <> replicate (numOutputBits - length bin) 0
            selected = take (zNum + 1) (padded target)
            valueMap = toValueMapFor (padded xBin) 'x' <> toValueMapFor (padded yBin) 'y'
         in take (zNum + 1) (fst (findValues valueMap machine 'z')) == selected && go (n - 1) gen3

andColor :: Attribute
andColor = Color [WC (toColor Orange) Nothing]

orColor :: Attribute
orColor = Color [WC (toColor LightBlue) Nothing]

xorColor :: Attribute
xorColor = Color [WC (toColor Red) Nothing]

nodes :: Map Text (Gate, Text, Text) -> [DotNode Text]
nodes mapping = go (M.keys mapping) S.empty
  where
    go [] _ = []
    go (key : rest) visited
        | key `S.member` visited = go rest visited
        | key `M.notMember` mapping = DotNode key [Color [WC (toColor Black) Nothing], Rank SourceRank] : go rest (S.insert key visited)
        | otherwise =
            let (gate, inputA, inputB) = mapping M.! key
                c = case gate of
                    AND -> andColor
                    OR -> orColor
                    XOR -> xorColor
                d = if "z" `T.isPrefixOf` key then [Rank SinkRank] else []
             in DotNode key (c : d) : go (inputA : inputB : rest) (S.insert key visited)

edges :: Map Text (Gate, Text, Text) -> [DotEdge Text]
edges = go . M.toList
  where
    go [] = []
    go ((output, (_, inputA, inputB)) : rest) = DotEdge inputA output [] : DotEdge inputB output [] : go rest


-- | part 2 was done by manually inspecting output/d24.png and using the test machine function
-- part2 :: String -> [Int]
-- part2 s = createGraph outputMap
-- part2 s = runGraphvizCommand Dot (createGraph outputMap) Png "output/d24.png"
-- part2 s = runGraphvizCommand Dot (mkGraph (nodes outputMap) (edges outputMap)) Png "output/d24.png"
-- part2 s = runGraphvizCommand Dot dg Png "output/d24.png"
part2 :: String -> IO String
part2 s = print (testMachineFor outputMap 46 45) >> pure ""
  where
    dg = toCanonical $ mkGraph (nodes outputMap) (edges outputMap)
    st = graphStatements dg
    att = attrStmts st
    ndg = dg{graphStatements = st{attrStmts = [GraphAttrs [RankDir FromTop]]}}
    (_valueMap, outputMap) = pInput s
    -- (xBinValue, aMap) = findValues valueMap outputMap 'x'
    -- (yBinValue, bMap) = findValues aMap outputMap 'y'
    -- (zBinValue, cMap) = findValues bMap outputMap 'z'
    -- target = intToBin (binToInt xBinValue + binToInt yBinValue)
    -- targetBin = target <> replicate (length zBinValue - length target) 0
    -- cValueMap = M.insert "y01" 1 $ M.insert "y00" 1 $ M.insert "x00" 1 $ M.map (const 0) valueMap



-- swapPair :: Map Text (Gate, Text, Text) -> (Text, Text) -> Map Text (Gate, Text, Text)
-- swapPair outputMap (keyA, keyB) = newMap
--   where
--     oldValA = outputMap M.! keyA
--     oldValB = outputMap M.! keyB
--     newMap = M.insert keyA oldValB $ M.insert keyB oldValA outputMap

-- gatesToBaseDepends :: Map Text (Gate, Text, Text) -> Map Text (Set Int) -> Text -> (Set Int, Map Text (Set Int))
-- gatesToBaseDepends outputMap cache target
--     | "x" `T.isPrefixOf` target || "y" `T.isPrefixOf` target =
--         let v = read (drop 1 $ T.unpack target)
--             s = S.singleton v
--          in (s, M.insert target s cache)
--     | otherwise = case M.lookup target cache of
--         Just v -> (v, cache)
--         Nothing -> (combined, M.insert target combined wCache)
--   where
--     (_, keyA, keyB) = outputMap M.! target
--     (resA, vCache) = gatesToBaseDepends outputMap cache keyA
--     (resB, wCache) = gatesToBaseDepends outputMap vCache keyB
--     combined = resA <> resB


-- fixMachine :: Map Text (Gate, Text, Text) -> Map Text (Gate, Text, Text)
-- fixMachine originalMachine = go 0 originalMachine S.empty
--   where
--     numOutputBits = length $ filter ("z" `T.isPrefixOf`) $ M.keys originalMachine
--     go zNum machine visited
--         | zNum == numOutputBits + 1 = machine
--         | testMachineFor machine numOutputBits zNum = go (zNum + 1) machine visited
--         | otherwise = go (zNum + 1) fixedMachine (S.singleton (cText zNum 'z') <> visited <> wiresAffecting fixedMachine (cText zNum 'z'))
--       where
--         filteredMachine = M.filterWithKey (\key _ -> key `S.notMember` visited) machine
--         fixedMachine = findWorkingSwappedMachine machine (allPairs filteredMachine) zNum
--     findWorkingSwappedMachine machine [] _ = machine
--     findWorkingSwappedMachine machine (pair : rest) zNum
--         | testMachineFor swapped numOutputBits zNum = swapped
--         | otherwise = findWorkingSwappedMachine machine rest zNum
--       where
--         swapped = swapPair machine pair

-- fixMachineAt :: Map Text (Gate, Text, Text) -> Int -> Map Text (Gate, Text, Text)
-- fixMachineAt originalMachine zNum
--     | testMachineFor originalMachine numOutputBits zNum = originalMachine
--     | otherwise = findWorkingSwappedMachine originalMachine (allPairs originalMachine)
--   where
--     numOutputBits = length $ filter ("z" `T.isPrefixOf`) $ M.keys originalMachine
--     findWorkingSwappedMachine machine [] = machine
--     findWorkingSwappedMachine machine (pair : rest)
--         | testMachineFor swapped numOutputBits zNum = swapped
--         | otherwise = findWorkingSwappedMachine machine rest
--       where
--         swapped = swapPair machine pair


-- wiresAffecting :: Map Text (Gate, Text, Text) -> Text -> Set Text
-- wiresAffecting outputMap target = go [outputMap M.! target] S.empty
--   where
--     go [] visited = visited
--     go ((_, a, b) : xs) visited = go xs (S.insert a $ S.insert b visited)

-- outputAffectedBySwapping :: Map Text (Gate, Text, Text) -> Text -> Text -> Set Text
-- outputAffectedBySwapping outputMap one two =
--     S.insert one $
--         S.insert two $
--             foldr (<>) mempty $
--                 map (S.filter (`M.member` outputMap)) [wiresAffecting outputMap one, wiresAffecting outputMap two]

-- type Pair = (Text, Text)

-- allPairs :: Map Text (Gate, Text, Text) -> [(Text, Text)]
-- allPairs = go . M.keys
--   where
--     go [] = []
--     go (x : xs) = [(x, y) | y <- xs] <> go xs

-- allSwappableFourPairs :: Map Text (Gate, Text, Text) -> Set (Pair, Pair, Pair, Pair)
-- allSwappableFourPairs outputMap = S.fromList $ nubByUnique $ go $ M.keys outputMap
--   where
--     func ((ax, ay), (bx, by), (cx, cy), (dx, dy)) = concat $ sort [sort [ax, ay], sort [bx, by], sort [cx, cy], sort [dx, dy]]
--     nubByUnique :: [(Pair, Pair, Pair, Pair)] -> [(Pair, Pair, Pair, Pair)]
--     nubByUnique = nubOrdOn func
--     go m = do
--         a <- m
--         b <- filter (/= a) m
--         c <- filter (`notElem` [a, b]) m
--         d <- filter (`notElem` [a, b, c]) m
--         e <- filter (`notElem` [a, b, c, d]) m
--         f <- filter (`notElem` [a, b, c, d, e]) m
--         g <- filter (`notElem` [a, b, c, d, e, f]) m
--         h <- filter (`notElem` [a, b, c, d, e, f, g]) m
--         pure ((a, b), (c, d), (e, f), (g, h))

-- findSwaps :: Map Text Int -> Map Text (Gate, Text, Text) -> [Int] -> [Pair]
-- findSwaps valueMap oOutputMap tBin = go oOutputMap 0 [] S.empty
--   where
--     go outputMap idx swappedPairs swapped
--         | length swappedPairs > 4 = []
--         | idx == length tBin =
--             if fst (findValues valueMap outputMap 'z') == tBin
--                 then swappedPairs
--                 else []
--         | fst (findValue valueMap outputMap (cText idx 'z')) == tBin !! idx = go outputMap (idx + 1) swappedPairs swapped
--         | otherwise = do
--             pair@(keyA, keyB) <- filter (\pair@(a, b) -> a `S.notMember` swapped && b `S.notMember` swapped && (pair `notElem` swappedPairs)) $ allPairs outputMap
--             let newMap = swapPair outputMap pair
--                 (value, _) = findValue valueMap newMap (cText idx 'z')
--             guard (value == tBin !! idx)
--             go newMap (idx + 1) ((keyA, keyB) : swappedPairs) (S.insert keyA $ S.insert keyB swapped)
