{-# LANGUAGE OverloadedStrings #-}

module Days.D17 (run, part1, part2) where

import Control.Concurrent
import Control.Monad
import Control.Parallel.Strategies (parListChunk, parMap, rdeepseq, rpar, using)
import Data.Bits
import Data.Containers.ListUtils (nubOrd)
import Data.Heap (Heap)
import Data.Heap qualified as H
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Ix
import Data.List (elemIndex, find, intercalate, intersperse, sort, (\\))
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Float.RealFracMethods (ceilingDoubleInt, floorDoubleInt)
import Text.Megaparsec (Parsec, optional, parseMaybe, sepBy)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer qualified as L

run :: IO ()
run = do
    setNumCapabilities 32
    input <- readFile "input/d17.txt"
    print $ part1 input
    print $ part2 input

data Operation
    = ADV
    | BXL
    | BST
    | JNZ
    | BXC
    | OUT
    | BDV
    | CDV
    deriving (Show, Eq, Ord, Enum)

compoundOp :: Operation -> Computer -> Int
compoundOp op comp
    | inRange (0, 3) literal = literal
    | literal == 4 = regA comp
    | literal == 5 = regB comp
    | literal == 6 = regC comp
    | otherwise = 0
  where
    literal = fromEnum op

data Computer = Computer
    { cProgram :: [Operation]
    , cPointer :: Int
    , regA :: Int
    , regB :: Int
    , regC :: Int
    , output :: [Int]
    }
    deriving (Show)

runProgram :: Maybe Int -> Computer -> Computer
runProgram earlyTermination comp =
    let program = cProgram comp
        pointer = cPointer comp
        op = program !! pointer
        operand = program !! (pointer + 1)
        newPointer = pointer + 2
        newComp = case op of
            ADV ->
                let numerator = regA comp
                    denominator = 2 ^ (compoundOp operand comp)
                    result = numerator `div` denominator
                 in comp{regA = result, cPointer = newPointer}
            BXL ->
                let result = regB comp `xor` fromEnum operand
                 in comp{regB = result, cPointer = newPointer}
            BST ->
                let result = compoundOp operand comp `mod` 8
                 in comp{regB = result, cPointer = newPointer}
            JNZ ->
                let value = regA comp
                    target = fromEnum operand
                 in if value == 0
                        then comp{cPointer = newPointer}
                        else comp{cPointer = target}
            BXC ->
                let result = regB comp `xor` regC comp
                 in comp{regB = result, cPointer = newPointer}
            OUT ->
                let result = compoundOp operand comp `mod` 8
                 in comp{output = result : output comp, cPointer = newPointer}
            BDV ->
                let numerator = regA comp
                    denominator = 2 ^ (compoundOp operand comp)
                    result = numerator `div` denominator
                 in comp{regB = result, cPointer = newPointer}
            CDV ->
                let numerator = regA comp
                    denominator = 2 ^ (compoundOp operand comp)
                    result = numerator `div` denominator
                 in comp{regC = result, cPointer = newPointer}
     in if pointer >= length program - 1 || (isJust earlyTermination && fromJust earlyTermination <= length (output comp))
            then comp
            else runProgram earlyTermination newComp

type Parser = Parsec Void Text

pComputer :: Parser Computer
pComputer = do
    rA <- string "Register A: " *> L.decimal <* eol
    rB <- string "Register B: " *> L.decimal <* eol
    rC <- string "Register C: " *> L.decimal <* eol
    void eol
    program <- string "Program: " *> sepBy L.decimal (char ',')
    pure
        Computer
            { cProgram = map toEnum program
            , cPointer = 0
            , regA = rA
            , regB = rB
            , regC = rC
            , output = []
            }

pInput :: String -> Computer
pInput = fromJust . parseMaybe (pComputer <* optional eol) . T.pack

-- part1 :: String -> String
part1 s = intercalate "," . map show . reverse . output $ runProgram Nothing comp
  where
    -- part1 s = runProgram comp
    -- part1 s = comp

    comp = pInput s

findA :: Computer -> Int
-- findA comp = go (10 ^ length (cProgram comp))
findA comp = go (10 ^ length target)
  where
    -- target = map fromEnum (cProgram comp)
    target = reverse $ map fromEnum (cProgram comp)
    early = Just $ length target
    -- getOutput = reverse . output
    getOutput = output
    success = (== target) . getOutput
    n = 10000
    go a
        | any id compd = a + fromJust (elemIndex True compd)
        | otherwise = go (a + n + 1)
      where
        compd = parMap rdeepseq (\nA -> success $ runProgram early comp{regA = nA}) [a .. a + n]

-- compd = flip using (parListChunk 1000 rdeepseq) $ map (\nA -> success $ runProgram early comp{regA = nA}) [a .. a + n]

toBaseList :: Int -> Int -> [Int]
toBaseList n b = reverse $ go n
  where
    go 0 = []
    go x = (x `mod` b) : toBaseList (x `div` b) b

fromBase :: Int -> Int -> Int
fromBase n b = go n 0
  where
    go 0 _ = 0
    go x e = (x `mod` 10) * (b ^ (e :: Int)) + go (x `div` 10) (e + 1)

g :: Int -> Int -> Int
g a e = a * (10 ^ e)

digitsToNum :: [Int] -> Int
digitsToNum lst = sum $ zipWith g lst (reverse [0 .. length lst])

-- findAB :: Computer -> Int
-- findAB comp = digitsToNum $ head $ combinations alphabet
findAB comp = go valuesToTry
  where
    target = reverse $ map fromEnum (cProgram comp)
    early = Just $ length target
    -- getOutput = reverse . output
    getOutput = output
    success = (== target)
    n = 10000
    alphabet = sort $ map fromEnum $ cProgram comp
    combinations :: [Int] -> [[Int]]
    combinations [] = [[]]
    combinations lst = concat [flip using (parListChunk 100000 rpar) $ nubOrd $ map (f :) $ combinations (lst \\ [f]) | f <- lst]
    valuesToTry = map (flip fromBase 8 . digitsToNum) (combinations alphabet)
    go [] = 0
    go lst = case find success compd of
        Nothing -> go (drop n lst)
        Just val -> flip fromBase 8 $ digitsToNum val
      where
        -- go lst = take 5 compd

        compd = parMap rpar (\nA -> getOutput $ runProgram early comp{regA = nA}) (take n lst)

-- compd = parMap rpar (\nA ->  runProgram early comp{regA = nA}) (take n lst)

oneLoop :: Int -> (Int, Int)
oneLoop a = (a', b'''' `mod` 8)
  where
    b' = a `mod` 8 -- littlest 3 bits of a
    b'' = b' `xor` 2 -- flip 2nd bit
    c' = a `div` (2 ^ b'') -- shift a by b bits right and store in c
    b''' = b'' `xor` 3 -- flip 3rd bit
    b'''' = b''' `xor` c' -- xor b with c: outputted
    a' = a `div` 8 -- discard littlest 3 bits of a

oneLoopD :: Int -> Int
oneLoopD a = b'''' `mod` 8
  where
    b' = a `mod` 8 -- littlest 3 bits of a
    b'' = b' `xor` 2 -- flip 2nd bit
    c' = a `div` (2 ^ b'') -- shift a by b bits right and store in c
    b''' = b'' `xor` 3 -- flip 3rd bit
    b'''' = b''' `xor` c' -- xor b with c: outputted
    a' = a `div` 8 -- discard littlest 3 bits of a

doAll :: Int -> Int -> Int -> [Int]
doAll 0 _ _ = []
doAll a b c = let (a', val) = oneLoop a in val : doAll a' b c

waysToMake :: Int -> [Int]
waysToMake digit = filter ((== digit) . snd . oneLoop) [0 .. 2 ^ (10 :: Int) - 1]

isOverlapping :: Int -> Int -> Bool
isOverlapping before after = a == b
  where
    m = (1 .<<. 7) - 1
    a = after .&. m
    b = before .>>. 3

calculateAll :: [Int] -> IntMap [Int]
calculateAll = IM.fromList . map ((,) <$> id <*> waysToMake) . nubOrd

part2 :: String -> Int
part2 s = go initialHeap allMapping
  where
    comp = pInput s
    program = map fromEnum $ cProgram comp
    revProg = reverse program
    initialHeap :: Heap (H.Entry Int (Int, [Int]))
    allMapping = calculateAll program
    -- initialHeap = H.fromList $ map (H.Entry <$> id <*> (,drop 1 program)) (allMapping IM.! (head program))
    initialHeap = H.fromList $ map (H.Entry <$> id <*> (,drop 1 revProg)) (allMapping IM.! (head revProg))
    target = program
    go heap mapping
        | H.null heap = 0
        -- \| null remaining = if doAll num 0 0 == target then num else go newHeap mapping
        | null remaining = num
        | otherwise = go (newHeap <> newStates) mapping
      where
        minState = H.minimum heap
        num = H.priority minState
        (current, remaining) = H.payload minState
        newHeap = H.deleteMin heap
        adjacentN = filter (flip isOverlapping current) $ mapping IM.! (head remaining)
        -- adjacent = filter (isOverlapping current) $ mapping IM.! (head remaining)
        f x =
            let y = x .>>. 7
                -- z = y .<<. 7
                beta = (ceilingDoubleInt (logBase 2 (fromIntegral num)))
                alpha = y .<<. beta
                gamma = alpha .|. num
                epsilon = 1 .<<. (3 * 16)
             in H.Entry (gamma .&. (epsilon - 1)) (x, drop 1 remaining)
        newStates = H.fromList $ map f adjacentN

adjacentNum :: Int -> [Int]
adjacentNum larger = [alpha .|. gamma | gamma <- [0 .. (1 .<<. 3) - 1]]
  where
    alpha = larger .<<. 3

findMatching :: [Int] -> IO Int
findMatching program = print start >> go start
  where
    start = map (,drop 1 $ reverse program) $ filter ((==0) .oneLoopD) [0..(1.<<. 3) - 1]
    go [] = pure 0
    go ((num, prog) : xs)
        | null prog = pure num
        | otherwise = print newStates >> go (xs <> newStates)
      where
        p = head prog
        newStates = map (,drop 1 prog) . filter ((== p) . oneLoopD) $ adjacentNum num