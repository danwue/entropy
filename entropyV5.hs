#!/usr/bin/env runhaskell

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TupleSections #-}

import Data.Array (Ix)
import Data.Array.Base (UArray, castSTUArray)
import Data.Array.IArray
import Data.Bits (Bits (shiftL, shiftR), FiniteBits (countTrailingZeros), (.&.), (.|.))
import Data.Char (isAsciiUpper, toLower, toUpper)
import Data.Foldable (Foldable (foldr'), maximumBy, minimumBy)
import Data.Function (on)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (dropWhileEnd, group, inits, intersperse, partition, sort, sortOn, tails, transpose)
import Data.Map (Map, findWithDefault, fromListWith, toList)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Data.Ord (Down (Down))
import Data.Word (Word32, Word8)
import Debug.Trace (trace, traceShow)
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)

--- Game Tree ---

data GameTree move state = GameTree state [(move, GameTree move state)] deriving (Show)

instance Functor (GameTree move) where
  fmap f (GameTree state xs) = GameTree (f state) $ mapSnd (fmap f) xs

mapSnd :: (t -> b) -> [(a, t)] -> [(a, b)]
mapSnd f [] = []
mapSnd f ((a, b) : xs) = (a, f b) : mapSnd f xs

minimumOn :: Ord a => (c -> a) -> [c] -> c
minimumOn f = snd . minimumBy (compare `on` fst) . map (\x -> (f x, x))

maximumOn :: Ord a => (c -> a) -> [c] -> c
maximumOn f = snd . maximumBy (compare `on` fst) . map (\x -> (f x, x))

build :: (state -> [(move, state)]) -> state -> GameTree move state
build f s = GameTree s $ mapSnd (build f) $ f s

limitDepth :: Int -> GameTree move state -> GameTree move state
limitDepth 0 (GameTree state children) = GameTree state []
limitDepth d (GameTree state children) = GameTree state $ mapSnd (limitDepth (d -1)) children

state :: GameTree move state -> state
state (GameTree state _) = state

--- Entropy Game ---

type Line = Word32

data LineType = Row | Col deriving (Ix, Eq, Ord, Show)

data Index = A | B | C | D | E | F | G deriving (Show, Ix, Eq, Ord, Enum, Read)

data Counter = Blue | Yellow | Red | Green | Pink | Maroon | Black deriving (Enum, Show, Eq, Ord)

remove :: Index -> Line -> Line
remove A = (.&. 0b111111111111111111000)
remove B = (.&. 0b111111111111111000111)
remove C = (.&. 0b111111111111000111111)
remove D = (.&. 0b111111111000111111111)
remove E = (.&. 0b111111000111111111111)
remove F = (.&. 0b111000111111111111111)
remove G = (.&. 0b000111111111111111111)

add :: Counter -> Index -> Line -> Line
add c i = (.|. (((fromIntegral . fromEnum) c + 1) `shiftL` (3 * fromEnum i)))

get :: Index -> Line -> Maybe Counter
get i l = d $ (l `shiftR` (3 * fromEnum i)) .&. 0b111
  where
    d 0 = Nothing
    d i = Just $ toEnum . fromIntegral $ i -1

move' :: Index -> Index -> Line -> Line
move' s t r = (remove s . add c t) r
  where
    Just c = get s r

partition' :: Line -> ([Index], [Index])
partition' l = (map fst empty, map fst occupied)
  where
    cs = iterate (`shiftR` 3) l
    (empty, occupied) = partition ((== 0) . (.&. 0b111) . snd) $ [A .. G] `zip` cs

empty' :: Line -> [Index]
empty' = fst . partition'

occupied' :: Line -> [Index]
occupied' = snd . partition'

moves' :: Line -> [(Index, Index)]
moves' = f . partition'
  where
    f ([], _) = []
    f (_, []) = []
    f (t : ts, s : ss) | t < s = (s, t) : f (ts, s : ss)
    f (t : ts, s1 : s2 : ss)
      | t < s2 = (s1, t) : (s2, t) : f (ts, s1 : s2 : ss)
      | otherwise = f (t : ts, s2 : ss)
    f (t : ts, s : ss) = (s, t) : f (ts, s : ss)

type Square = (Index, Index)

type Board = UArray (LineType, Index) Line

type Score = Float

data Move = Pick Counter | Place Square | Move Square Square deriving (Show)

data GameState = AwaitingChaos Counter Board | AwaitingRandom Board | AwaitingOrder Board deriving (Eq, Show)

board :: GameState -> Board
board (AwaitingChaos _ board) = board
board (AwaitingOrder board) = board
board (AwaitingRandom board) = board

move :: GameState -> Move -> GameState
move (AwaitingRandom b) (Pick counter) = AwaitingChaos counter b
move (AwaitingChaos counter b) (Place (row, col)) = AwaitingOrder $ b // [((Row, row), add counter col (b ! (Row, row))), ((Col, col), add counter row (b ! (Col, col)))]
move (AwaitingOrder b) a@(Move src@(srcRow, srcCol) dst@(dstRow, dstCol))
  | src == dst = AwaitingRandom b
  | srcRow == dstRow = AwaitingRandom $ b // [((Row, srcRow), (remove srcCol . add c dstCol) (b ! (Row, srcRow))), ((Col, srcCol), remove srcRow (b ! (Col, srcCol))), ((Col, dstCol), add c dstRow (b ! (Col, dstCol)))]
  | srcCol == dstCol = AwaitingRandom $ b // [((Col, srcCol), (remove srcRow . add c dstRow) (b ! (Col, srcCol))), ((Row, srcRow), remove srcCol (b ! (Row, srcRow))), ((Row, dstRow), add c dstCol (b ! (Row, dstRow)))]
  | otherwise = error $ "Illegal move action:" ++ show a
  where
    Just c = get srcCol $ b ! (Row, srcRow)
move state action = error $ "Illegal action for given game state: " ++ show action

emptySquares :: Board -> [Square]
emptySquares b = [(r, c) | r <- [A .. G], c <- empty' $ b ! (Row, r)]

occupiedSquares :: Board -> [Square]
occupiedSquares b = [(r, c) | r <- [A .. G], c <- occupied' $ b ! (Row, r)]

skipMove :: Board -> Move
skipMove board = Move occupiedSquare occupiedSquare
  where
    occupiedSquare = head . occupiedSquares $ board

moves :: GameState -> [Move]
moves (AwaitingChaos counter b) = map Place $ emptySquares b
moves (AwaitingRandom b) = [Pick c | c <- [Blue .. Black]]
moves (AwaitingOrder b)
  | null rows && null cols = [] -- game over
  | otherwise = rows ++ cols ++ [skipMove b]
  where
    rows = [Move (r, srcCol) (r, dstCol) | r <- [A .. G], (srcCol, dstCol) <- moves' (b ! (Row, r))]
    cols = [Move (srcRow, c) (dstRow, c) | c <- [A .. G], (srcRow, dstRow) <- moves' (b ! (Col, c))]

-- Heuristic --

continuousSubSeqs :: [a] -> [[a]]
continuousSubSeqs xs = [ts | is <- inits xs, ts <- tails is]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- each square can be in 8 different states (empty, or containing counter 1-7)
-- thus, we can encode a row/column into an integer to have a compact lookup key.
-- in total, we have an integer with maximum value of 2^21-1 (7 squares * 3 bits/square = 21 bits)
intToLine :: Line -> [Maybe Counter]
intToLine 0 = []
intToLine i = (d . (.&. 0b111)) i : intToLine (i `shiftR` 3)
  where
    d 0 = Nothing
    d i = Just $ toEnum . fromIntegral $ i -1

-- Values (unlike functions) are cached in Haskell. We can exploit this and use a simple array as lazy initialized cache.
lineScore :: UArray Line Score
lineScore = array (0, 0b111111111111111111111) [(key, lineScore $ intToLine key) | key <- [0 .. 0b111111111111111111111]]
  where
    lineScore :: [Maybe Counter] -> Score
    lineScore xs = sum . map score . filter isPalindrome $ continuousSubSeqs xs
      where
        -- non-patterns
        score (Nothing : xs) = 0
        -- odd patterns
        score [a, Nothing, b] = 1 + score [a, b]
        score [a, b, Nothing, c, d] = 1 + score [a, b, c, d]
        score [a, b, c, Nothing, d, e, f] = 1 + score [a, b, c, d, e, f]
        -- even patterns
        score p = fromIntegral . length . filter isJust $ p

score :: GameState -> Score
score = sum . elems . amap (lineScore !) . board

-- alpha: maximum points order can force
-- beta: minimum points beta can force
alphaBeta :: GameTree Move GameState -> Move
alphaBeta gt = traceShow s m
  where
    posInf = 1.0 / 0
    negInf = -1.0 / 0
    (Just m, s) = alphaBeta' negInf posInf gt
    alphaBeta' :: Score -> Score -> GameTree Move GameState -> (Maybe Move, Score)
    alphaBeta' _ _ (GameTree s []) = (Nothing, score s)
    alphaBeta' a b (GameTree (AwaitingOrder _) cs) = acc (Nothing, negInf) a cs
      where
        acc (m, v) a [] = (m, v)
        acc (m, v) a _ | v >= b = (m, v) -- cutoff
        acc (m, v) a ((m', s) : xs)
          | v < v' = acc (Just m', v') (max a v') xs
          | otherwise = acc (m, v) a xs
          where
            v' = snd (alphaBeta' a b s)
    alphaBeta' a b (GameTree (AwaitingChaos _ _) cs) = acc (Nothing, posInf) b cs
      where
        acc (m, v) b [] = (m, v)
        acc (m, v) b _ | v <= a = (m, v) -- cutoff
        acc (m, v) b ((m', s) : xs)
          | v > v' = acc (Just m', v') (min b v') xs
          | otherwise = acc (m, v) b xs
          where
            v' = snd (alphaBeta' a b s)
    alphaBeta' a b (GameTree (AwaitingRandom board) cs) = (Nothing,) . (/ fromIntegral emptyFields) . sum $ [snd (alphaBeta' a b gt) * fromIntegral (findWithDefault 7 c counts) | (m@(Pick c), gt) <- cs, findWithDefault 7 c counts > 0]
      where
        emptyFields = 49 - length placedCounters
        counts = Map.map (7 -) . fromListWith (+) . map (,1) $ placedCounters
        placedCounters = catMaybes $ concatMap intToLine [board ! (Row, r) | r <- [A .. G]]

entropyTree :: GameState -> GameTree Move GameState
entropyTree gs = build f gs
  where
    f gs = [(m, move gs m) | m <- moves gs]

makeTurn :: GameState -> Move
makeTurn gs@(AwaitingChaos _ _) = alphaBeta . limitDepth 4 . entropyTree $ gs
makeTurn gs@(AwaitingOrder _) = alphaBeta . limitDepth 4 . entropyTree $ gs
makeTurn gs@(AwaitingRandom _) = error "illegal state"

-- All IO stuff --

parseCounter :: String -> Counter
parseCounter = toEnum . (+ (-1)) . read

parseSq :: String -> Square
parseSq [r, c] = (read [r], read [toUpper c])
parseSq s = error $ "Illegal square identifier: " ++ s

printSq :: Square -> String
printSq (r, c) = show r ++ map toLower (show c)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  receiveInput $ AwaitingRandom $ listArray ((Row, A), (Col, G)) $ repeat 0

receiveInput :: GameState -> IO ()
receiveInput gs = do
  cmd <- getLine
  handleInput cmd gs

handleInput :: String -> GameState -> IO ()
handleInput "Start" gs = receiveInput gs
handleInput "Quit" gs = return ()
handleInput [counter] gs | counter `elem` ['1' .. '7'] = do
  let gs' = move gs (Pick (parseCounter [counter]))
  let a@(Place sq) = makeTurn gs'
  putStrLn . printSq $ sq
  receiveInput $ move gs' a
handleInput [counter, row, col] gs | counter `elem` ['1' .. '7'] && row `elem` ['A' .. 'G'] && col `elem` ['a' .. 'g'] = do
  let gs' = move gs (Pick (parseCounter [counter]))
  let gs'' = move gs' (Place (parseSq [row, col]))
  let a@(Move src dst) = makeTurn gs''
  putStrLn . concatMap printSq $ [src, dst]
  receiveInput $ move gs'' a
handleInput [srcRow, srcCol, dstRow, dstCol] gs | all (`elem` ['A' .. 'G']) [srcRow, dstRow] && all (`elem` ['a' .. 'g']) [srcCol, dstCol] = do
  let gs' = move gs (Move (parseSq [srcRow, srcCol]) (parseSq [dstRow, dstCol]))
  receiveInput gs'
handleInput cmd gs = do
  hPutStrLn stderr $ "Illegal command: " ++ cmd
  receiveInput gs