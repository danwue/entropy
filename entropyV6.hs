#!/usr/bin/env runhaskell

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TupleSections #-}

import Data.Array (Ix)
import Data.Array.Base (UArray)
import Data.Array.IArray
  ( Array,
    amap,
    array,
    elems,
    listArray,
    (!),
    (//),
  )
import Data.Bits (Bits (shiftL, shiftR, (.&.), (.|.)))
import Data.Char (toLower, toUpper)
import Data.List (inits, partition, tails)
import Data.Map (Map, findWithDefault, fromListWith)
import Data.Maybe (catMaybes, isJust)
import Data.Word (Word32)
import System.IO
  ( BufferMode (LineBuffering),
    hPutStrLn,
    hSetBuffering,
    stderr,
    stdout,
  )

--- Game Tree ---

data GameTree move state = GameTree state [(move, GameTree move state)] deriving (Show)

mapSnd :: (t -> b) -> [(a, t)] -> [(a, b)]
mapSnd f [] = []
mapSnd f ((a, b) : xs) = (a, f b) : mapSnd f xs

build :: (state -> [(move, state)]) -> state -> GameTree move state
build f s = GameTree s $ mapSnd (build f) $ f s

limitDepth :: Int -> GameTree move state -> GameTree move state
limitDepth 0 (GameTree state children) = GameTree state []
limitDepth d (GameTree state children) = GameTree state $ mapSnd (limitDepth (d -1)) children

--- Entropy Game ---

-- main idea of encoding the board rows and columns into integer for fast score lookup:
-- each square can be in 8 different states (empty, or containing token 1-7)
-- thus, we can encode a row/column into an integer to have a compact lookup key.
-- in total, we have an integer with maximum value of 2^21-1 (7 squares * 3 bits/square = 21 bits)
type Line = Word32
data LineType = Row | Col deriving (Ix, Eq, Ord, Show)
data Index = A | B | C | D | E | F | G deriving (Show, Ix, Eq, Ord, Enum, Read)
data Token = Blue | Yellow | Red | Green | Pink | Maroon | Black deriving (Enum, Show, Eq, Ord)

lineToTokens :: Line -> [Maybe Token]
lineToTokens 0 = []
lineToTokens i = (d . (.&. 0b111)) i : lineToTokens (i `shiftR` 3)
  where
    d 0 = Nothing
    d i = Just $ toEnum . fromIntegral $ i -1

remove :: Index -> Line -> Line
remove A = (.&. 0b111111111111111111000)
remove B = (.&. 0b111111111111111000111)
remove C = (.&. 0b111111111111000111111)
remove D = (.&. 0b111111111000111111111)
remove E = (.&. 0b111111000111111111111)
remove F = (.&. 0b111000111111111111111)
remove G = (.&. 0b000111111111111111111)

add :: Token -> Index -> Line -> Line
add c i = (.|. (((fromIntegral . fromEnum) c + 1) `shiftL` (3 * fromEnum i)))

get :: Index -> Line -> Maybe Token
get i l = d $ (l `shiftR` (3 * fromEnum i)) .&. 0b111
  where
    d 0 = Nothing
    d i = Just $ toEnum . fromIntegral $ i -1

-- parititons line into empty and occupied indexes
partitionLine :: Line -> ([Index], [Index])
partitionLine l = (map fst empty, map fst occupied)
  where
    cs = iterate (`shiftR` 3) l
    (empty, occupied) = partition ((== 0) . (.&. 0b111) . snd) $ [A .. G] `zip` cs

empty :: Line -> [Index]
empty = fst . partitionLine

occupied :: Line -> [Index]
occupied = snd . partitionLine

-- returns all moves possible within given row or column
lineMoves :: Line -> [(Index, Index)]
lineMoves = f . partitionLine
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
type Score = Float --  we use floats instead of integers because we use expectiminimax to calculate average outcomes.
data Move = Pick Token | Place Square | Move Square Square deriving (Show)
data GameState = AwaitingChaos Token Board | AwaitingRandom Board | AwaitingOrder Board deriving (Eq, Show)

board :: GameState -> Board
board (AwaitingChaos _ board) = board
board (AwaitingOrder board) = board
board (AwaitingRandom board) = board

move :: GameState -> Move -> GameState
move (AwaitingRandom b) (Pick token) = AwaitingChaos token b
move (AwaitingChaos token b) (Place (row, col)) = AwaitingOrder $ b // [((Row, row), add token col (b ! (Row, row))), ((Col, col), add token row (b ! (Col, col)))]
move (AwaitingOrder b) a@(Move src@(srcRow, srcCol) dst@(dstRow, dstCol))
  | src == dst = AwaitingRandom b
  | srcRow == dstRow = AwaitingRandom $ b // [((Row, srcRow), (remove srcCol . add c dstCol) (b ! (Row, srcRow))), ((Col, srcCol), remove srcRow (b ! (Col, srcCol))), ((Col, dstCol), add c dstRow (b ! (Col, dstCol)))]
  | srcCol == dstCol = AwaitingRandom $ b // [((Col, srcCol), (remove srcRow . add c dstRow) (b ! (Col, srcCol))), ((Row, srcRow), remove srcCol (b ! (Row, srcRow))), ((Row, dstRow), add c dstCol (b ! (Row, dstRow)))]
  | otherwise = error $ "Illegal move action:" ++ show a
  where
    Just c = get srcCol $ b ! (Row, srcRow)
move state action = error $ "Illegal action for given game state: " ++ show action

emptySquares :: Board -> [Square]
emptySquares b = [(r, c) | r <- [A .. G], c <- empty $ b ! (Row, r)]

occupiedSquares :: Board -> [Square]
occupiedSquares b = [(r, c) | r <- [A .. G], c <- occupied $ b ! (Row, r)]

-- dummy move for Order player which actually does not move any token
skipMove :: Board -> Move
skipMove board = Move occupiedSquare occupiedSquare
  where
    occupiedSquare = head . occupiedSquares $ board

moves :: GameState -> [Move]
moves (AwaitingChaos token b) = map Place $ emptySquares b
moves (AwaitingRandom b) = [Pick c | c <- [Blue .. Black]]
moves (AwaitingOrder b)
  | null rows && null cols = [] -- game over
  | otherwise = rows ++ cols ++ [skipMove b]
  where
    rows = [Move (r, srcCol) (r, dstCol) | r <- [A .. G], (srcCol, dstCol) <- lineMoves (b ! (Row, r))]
    cols = [Move (srcRow, c) (dstRow, c) | c <- [A .. G], (srcRow, dstRow) <- lineMoves (b ! (Col, c))]

-- Heuristic --

continuousSubSeqs :: [a] -> [[a]]
continuousSubSeqs xs = [ts | is <- inits xs, ts <- tails is]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- values (unlike functions) are cached in Haskell. We can exploit this and use a simple array as lazy initialized cache.
lineScore :: Array Line Score
lineScore = array (0, 0b111111111111111111111) [(key, lineScore $ lineToTokens key) | key <- [0 .. 0b111111111111111111111]]
  where
    lineScore :: [Maybe Token] -> Score
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

-- uses fail-soft alpha-beta pruning and expectiminimax to calculate expected outcome for random token choices.
-- see: https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning and https://en.wikipedia.org/wiki/Expectiminimax
alphaBeta :: GameTree Move GameState -> Move
alphaBeta gt = m
  where
    (Just m, s) = alphaBeta' negInf posInf gt
    posInf = 1.0 / 0
    negInf = -1.0 / 0
    alphaBeta' :: Score -> Score -> GameTree Move GameState -> (Maybe Move, Score)
    alphaBeta' _ _ (GameTree s []) = (Nothing, score s) -- leaf node
    alphaBeta' a b (GameTree (AwaitingOrder _) cs) = acc (Nothing, negInf) a cs -- maximizing player
      where
        acc (m, v) a [] = (m, v)
        acc (m, v) a _ | v >= b = (m, v) -- cutoff
        acc (m, v) a ((m', s) : xs)
          | v < v' = acc (Just m', v') (max a v') xs
          | otherwise = acc (m, v) a xs
          where
            v' = snd (alphaBeta' a b s)
    alphaBeta' a b (GameTree (AwaitingChaos _ _) cs) = acc (Nothing, posInf) b cs -- minimizing player
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
        emptyFields = 49 - length placedTokens
        counts = fmap (7 -) . fromListWith (+) . map (,1) $ placedTokens
        placedTokens = catMaybes $ concatMap lineToTokens [board ! (Row, r) | r <- [A .. G]]

entropyTree :: GameState -> GameTree Move GameState
entropyTree gs = build f gs
  where
    f gs = [(m, move gs m) | m <- moves gs]

makeTurn :: GameState -> Move
makeTurn gs@(AwaitingChaos _ _) = alphaBeta . limitDepth 4 . entropyTree $ gs
makeTurn gs@(AwaitingOrder _) = alphaBeta . limitDepth 4 . entropyTree $ gs
makeTurn gs@(AwaitingRandom _) = error "Illegal state"

-- All IO stuff --

parseToken :: String -> Token
parseToken = toEnum . (+ (-1)) . read

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
handleInput [token] gs | token `elem` ['1' .. '7'] = do
  let gs' = move gs (Pick (parseToken [token]))
  let a@(Place sq) = makeTurn gs'
  putStrLn . printSq $ sq
  receiveInput $ move gs' a
handleInput [token, row, col] gs | token `elem` ['1' .. '7'] && row `elem` ['A' .. 'G'] && col `elem` ['a' .. 'g'] = do
  let gs' = move gs (Pick (parseToken [token]))
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