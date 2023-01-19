module Entropy where

import Data.Array.Base (UArray, castSTUArray)
import Data.Array.IArray
import Data.Bits (Bits (shiftL, shiftR), (.&.))
import Data.Foldable (maximumBy, minimumBy, Foldable (foldr'))
import Data.Function (on)
import Data.Int (Int32, Int8)
import Data.List (inits, tails, sortOn, intersperse, dropWhileEnd)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout, hPutStrLn, stderr)
import Data.Ord (Down(Down))
import Distribution.Compat.CharParsing (CharParsing(char))
import Data.Char(ord, toLower, chr)

--- Game Tree ---

data GameTree move state = GameTree state [(move, GameTree move state)]

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
limitDepth d (GameTree state children) = GameTree state $ mapSnd (limitDepth (d-1)) children

limitStates :: Int -> GameTree move state -> GameTree move state
limitStates d (GameTree state children) | d <= 0 = GameTree state []
limitStates d (GameTree state children) = GameTree state $ take d $ mapSnd (limitStates d') children
  where
    d' = (d - length children) `div` length children

maximumState :: Ord state => GameTree move state -> state
maximumState (GameTree state []) = state
maximumState (GameTree _ children) = maximum $ map (minimumState . snd) children

minimumState :: Ord state => GameTree move state -> state
minimumState (GameTree state []) = state
minimumState (GameTree _ children) = minimum $ map (maximumState . snd) children

maximumMove :: Ord state => GameTree move state -> move
maximumMove (GameTree _ children) = fst $ maximumOn snd $ mapSnd minimumState children

minimumMove :: Ord state => GameTree move state -> move
minimumMove (GameTree _ children) = fst $ minimumOn snd $ mapSnd maximumState children


--- Entropy Game ---

type Square = (Char, Char)
type Counter = Int8
type Board = UArray Square Counter
data Move = Pick Counter | Place Counter Square | Move Square Square deriving (Show)
data GameState = AwaitingChaos Counter Board | AwaitingRandom Board | AwaitingOrder Board deriving (Eq, Show)


board :: GameState -> Board
board (AwaitingChaos _ board) = board
board (AwaitingOrder board) = board
board (AwaitingRandom board) = board


move :: GameState -> Move -> GameState
move (AwaitingRandom  board) (Pick counter) = AwaitingChaos counter board
move (AwaitingChaos _ board) (Place counter pos) = AwaitingOrder $ board // [(pos, counter)]
move (AwaitingOrder board) (Move src dst) = AwaitingRandom $ board // [(src, 0), (dst, board ! src)]
move state action = error.unlines $ ["Illegal action for given game state: " ++ show action, prettyPrint . board $ state]


remainingCounters :: Counter -> Board -> Int
remainingCounters counter = (7 -) . length . filter (== counter) . elems

emptySquares :: Board -> [Square]
emptySquares = map fst . filter ((== 0) . snd) . assocs

occupiedSquares :: Board -> [Square]
occupiedSquares = map fst . filter ((/= 0) . snd) . assocs

leftOf :: Square -> [Square]
leftOf (r, c) = reverse [(r, c') | c' <- ['a' .. pred c]]

rightOf :: Square -> [Square]
rightOf (r, c) = [(r, c') | c' <- [succ c .. 'g']]

upOf :: Square -> [Square]
upOf (r, c) = reverse [(r', c) | r' <- ['A' .. pred r]]

downOf :: Square -> [Square]
downOf (r, c) = [(r', c) | r' <- [succ r .. 'G']]

validMoves :: Square -> Board -> [Square]
validMoves sq board | board ! sq == 0 = []
validMoves sq board = concatMap (takeWhile (\sq' -> board ! sq' == 0)) [leftOf sq, rightOf sq, upOf sq, downOf sq]

skipMove :: Board -> Move
skipMove board = Move occupiedSquare occupiedSquare
  where
    occupiedSquare = head . occupiedSquares $ board

moves :: GameState -> [Move]
moves (AwaitingChaos counter board) = map (Place counter) $ emptySquares board
-- Lets only consider the two most likely colors, as we minimize the score anyway and it should be sufficient to block most moves
moves (AwaitingRandom board) = concat [moves $ AwaitingChaos counter board | counter <- take 2 . sortOn (Down . (`remainingCounters` board)) $ [1 .. 7], remainingCounters counter board > 0]
--moves (AwaitingChaos Nothing board) = concat [moves $ AwaitingChaos (Just counter) board | counter <- [1 .. 7], remainingCounters counter board > 0]
moves (AwaitingOrder board) = skipMove board : [Move src dst | src <- indices board, dst <- validMoves src board]


-- Heuristic --

-- instance Ord GameState where
--   s1 `compare` s2 = (heuristic . board) s1 `compare` (heuristic . board) s2
tilesWithCoordinates :: Board -> [(Counter, Int, Int)]
tilesWithCoordinates board =   concat [[(board ! (r, c) , charToInt $ r , charToInt $ c) | c <- ['a' .. 'g']] | r <- ['A' .. 'G']]

charToInt :: Char -> Int
charToInt c = (ord $ toLower c) - 98

intsToSquare :: (Int, Int) -> Square
intsToSquare (a, b) = (chr (a+66), chr (b+98))



continuousSubSeqs :: [a] -> [[a]]
continuousSubSeqs xs = [ts | is <- inits xs, ts <- tails is]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

patternScore :: [Counter] -> Int
patternScore xs = sum $ map score $ filter isPalindrome (continuousSubSeqs xs)
  where score [_] = 0 -- single counters do not give any score
        score (0:xs) = 0 -- ignore palindromes which start and end with empty squares
        score [_, 0, _] = 3 -- missing center pieces will always end up contributing to the score
        score [_, _, 0, _, _] = (+1) . length . filter (/= 0) $ xs
        score [_, _, _, 0, _,  _, _] = (+1) . length . filter (/= 0) $ xs
        score xs = length . filter (/= 0) $ xs -- just count all the counters on the squares

rows :: Board -> [[Counter]]
rows board = [[board ! (r, c) | c <- ['a' .. 'g']] | r <- ['A' .. 'G']]

rowHeuristic :: Char -> Board -> Int
rowHeuristic r board = (patternCache !) . patternToInt $ [board ! (r, c) | c <- ['a' .. 'g']]

colHeuristic :: Char -> Board -> Int
colHeuristic c board = (patternCache !) . patternToInt $ [board ! (r, c) | r <- ['A' .. 'G']]

heuristic :: Board -> HeuristicScore
heuristic board = (array ('A','G') rows, array ('a','h') cols)
  where rows = [ (r, rowHeuristic r board) | r <- ['A' .. 'G'] ]
        cols = [ (c, colHeuristic c board) | c <- ['a' .. 'g'] ]

type HeuristicScore = (UArray Char Int, UArray Char Int)

-- incremental update to the heuristic score based on the move
heuristic' :: HeuristicScore -> Move -> Board -> HeuristicScore
heuristic' score (Pick color) board = score
heuristic' (rowScore, colScore) (Place color (r,c)) board = (rowScore // [(r,row)], colScore // [(c,col)])
  where row = rowHeuristic r board
        col = colHeuristic c board
heuristic' (rowScore, colScore) (Move (srcRow, srcCol) (dstRow, dstCol)) board | srcRow == dstRow = (rowScore // [(srcRow,r)], colScore // [(srcCol,sc), (dstCol,dc)])
  where sc = colHeuristic srcCol board
        dc = colHeuristic dstCol board
        r = rowHeuristic srcRow board
heuristic' (rowScore, colScore) (Move (srcRow, srcCol) (dstRow, dstCol)) board | srcCol == dstCol =  (rowScore // [(srcRow,sr), (dstRow,dr)], colScore // [(srcCol,c)])
  where sr = rowHeuristic srcRow board
        dr = rowHeuristic dstRow board
        c = colHeuristic srcCol board
heuristic' _ move _ = error $ "Illegal action: " ++ show move

-- Hackish cache to avoid recalculating scores --

patternToInt :: [Counter] -> Int32
patternToInt = foldr (\e n -> (n `shiftL` 3) + fromIntegral e) 0


intToPattern :: Int32 -> [Counter]
intToPattern = take 7 . map (fromIntegral . (.&. 7)) . iterate (`shiftR` 3)

patternCache :: Array Int32 Int
patternCache = array (0, patternToInt $ replicate 7 7) [(key, patternScore $ intToPattern key) | key <- [0 .. (patternToInt $ replicate 7 7)]]

-- Choosing turn --

entropyTree :: GameState -> GameTree Move Int
entropyTree gs = (\(r,c) -> sum $ elems r ++ elems c) . snd <$> build f (gs, score)
  where
    f (gs,score) = [(m, (move gs m,heuristic' score m (board $ move gs m))) | m <- moves gs]
    score = heuristic . board $ gs

makeTurn :: GameState -> Move
makeTurn gs@(AwaitingChaos _ _) = minimumMove $ limitDepth 3 $ entropyTree gs
makeTurn gs@(AwaitingOrder _) = maximumMove $ limitDepth 3 $ entropyTree gs
makeTurn gs@(AwaitingRandom _) = error "illegal state"


-- All IO stuff --


prettyPrint :: Board -> String
prettyPrint board = map rep . unlines $ h:rs
  where h = "  " ++ intersperse ' ' ['a'..'g']
        rs = map (intersperse ' ') $ zipWith (:) ['A'..'G'] $ map (concatMap show) $ rows board
        rep '0' = ' '
        rep c = c



