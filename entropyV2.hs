#!/usr/bin/env runhaskell

import Data.Array.Base (UArray, castSTUArray)
import Data.Array.IArray
import Data.Bits (Bits (shiftL, shiftR), (.&.))
import Data.Foldable (maximumBy, minimumBy, Foldable (foldr'))
import Data.Function (on)
import Data.Int (Int32, Int8)
import Data.List (inits, tails, sortOn, intersperse, dropWhileEnd)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout, hPutStrLn, stderr)

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

state (GameTree state _) = state

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
data GameState = AwaitingChaos (Maybe Counter) Board | AwaitingOrder Board deriving (Eq)

board (AwaitingChaos _ board) = board
board (AwaitingOrder board) = board

move :: GameState -> Move -> GameState
move (AwaitingChaos _ board) (Pick counter) = AwaitingChaos (Just counter) board
move (AwaitingChaos _ board) (Place counter pos) = AwaitingOrder $ board // [(pos, counter)]
move (AwaitingOrder board) (Move src dst) = AwaitingChaos Nothing $ board // [(src, 0), (dst, board ! src)]
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
moves (AwaitingChaos (Just counter) board) = map (Place counter) $ emptySquares board
moves (AwaitingChaos Nothing board) = concat [moves $ AwaitingChaos (Just counter) board | counter <- [1 .. 7], remainingCounters counter board > 0]
moves (AwaitingOrder board) = skipMove board : [Move src dst | src <- indices board, dst <- validMoves src board]

-- Heuristic --

-- instance Ord GameState where
--   s1 `compare` s2 = (heuristic . board) s1 `compare` (heuristic . board) s2

rows :: Board -> [[Counter]]
rows board = [[board ! (r, c) | c <- ['a' .. 'g']] | r <- ['A' .. 'G']]

cols :: Board -> [[Counter]]
cols board = [[board ! (r, c) | r <- ['A' .. 'G']] | c <- ['a' .. 'g']]

continuousSubSeqs :: [a] -> [[a]]
continuousSubSeqs xs = [ts | is <- inits xs, ts <- tails is]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

patternScore :: [Counter] -> Int
patternScore xs = sum $ filter (> 1) $ map (length . filter (/= 0)) $ filter isPalindrome (continuousSubSeqs (dropWhileEnd (==0) . dropWhile (==0) $ xs))

heuristic :: Board -> Int
heuristic board = sum . map ((patternCache !) . patternToInt) $ rows board ++ cols board

-- incremental update to the heuristic
heuristic' :: Board -> Int -> Move -> Board -> Int
heuristic' prevBoard prevScore (Pick color) nextBoard = prevScore
heuristic' prevBoard prevScore (Place color (r,c)) nextBoard = prevScore - old + new
  where row = [(r,c) | c <- ['a'..'g']]
        col = [(r,c) | r <- ['A'..'G']]
        old = sum $ map ((patternCache !) . patternToInt . map (prevBoard !)) [row, col]
        new = sum $ map ((patternCache !) . patternToInt . map (nextBoard !)) [row, col]
heuristic' prevBoard prevScore (Move (srcRow, srcCol) (dstRow, dstCol)) nextBoard | srcRow == dstRow = prevScore - old + new
  where sc = [(r,srcCol) | r <- ['A'..'G']]
        dc = [(r,dstCol) | r <- ['A'..'G']]
        r = [(dstRow,c) | c <- ['a'..'g']]
        old = sum $ map ((patternCache !) . patternToInt . map (prevBoard !)) [r,sc,dc]
        new = sum $ map ((patternCache !) . patternToInt . map (nextBoard !)) [r,sc,dc]
heuristic' prevBoard prevScore (Move (srcRow, srcCol) (dstRow, dstCol)) nextBoard | srcCol == dstCol = prevScore - old + new
  where sr = [(srcRow,c) | c <- ['a'..'g']]
        dr = [(dstRow,c) | c <- ['a'..'g']]
        c = [(r,dstCol) | r <- ['A'..'G']]
        old = sum $ map ((patternCache !) . patternToInt . map (prevBoard !)) [sr,dr,c]
        new = sum $ map ((patternCache !) . patternToInt . map (nextBoard !)) [sr,dr,c]
heuristic' _ _ move _ = error $ "Illegal action: " ++ show move

-- Hackish cache to avoid recalculating scores --

patternToInt :: [Counter] -> Int32
patternToInt = foldr (\e n -> (n `shiftL` 3) + fromIntegral e) 0

intToPattern :: Int32 -> [Counter]
intToPattern = take 7 . map (fromIntegral . (.&. 7)) . iterate (`shiftR` 3)

patternCache :: Array Int32 Int
patternCache = array (0, patternToInt $ replicate 7 7) [(key, patternScore $ intToPattern key) | key <- [0 .. (patternToInt $ replicate 7 7)]]

-- Choosing turn --

entropyTree :: GameState -> GameTree Move (GameState, Int)
entropyTree gs = build f (gs, score)
  where
    f (gs,score) = [(m, (move gs m,heuristic' (board gs) score m (board $ move gs m))) | m <- moves gs]
    score = heuristic . board $ gs

makeTurn :: GameState -> Move
makeTurn gs@(AwaitingChaos _ _) = minimumMove $ fmap snd $ limitDepth 3 $ entropyTree gs
makeTurn gs@(AwaitingOrder _) = maximumMove $ fmap snd $ limitDepth 3 $ entropyTree gs

-- All IO stuff --

parseCounter :: String -> Counter
parseCounter = read

parseSq :: String -> Square
parseSq [r, c] = (r, c)
parseSq s = error $ "Illegal square identifier: " ++ s

printSq :: Square -> String
printSq (r, c) = [r, c]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  receiveInput $ AwaitingChaos Nothing $ listArray (('A', 'a'), ('G', 'g')) $ repeat 0

prettyPrint :: Board -> String
prettyPrint board = map rep . unlines $ h:rs
  where h = "  " ++ intersperse ' ' ['a'..'g']
        rs = map (intersperse ' ') $ zipWith (:) ['A'..'G'] $ map (concatMap show) $ rows board
        rep '0' = ' '
        rep c = c

receiveInput :: GameState -> IO ()
receiveInput gs = do
  --putStrLn . prettyPrint . board $ gs
  cmd <- getLine
  handleInput cmd gs

handleInput :: String -> GameState -> IO ()
handleInput "Start" gs = receiveInput gs
handleInput "Quit" gs = return ()
handleInput [counter] gs | counter `elem` ['1'..'7'] = do
  let gs' = move gs (Pick (parseCounter [counter]))
  let a@(Place counter sq) = makeTurn gs'
  putStrLn . printSq $ sq
  receiveInput $ move gs' a
handleInput [counter, row, col] gs | counter `elem` ['1'..'7'] && row `elem` ['A'..'G'] && col `elem` ['a'..'g'] = do
  let gs' = move gs (Place (parseCounter [counter]) (parseSq [row, col]))
  let a@(Move src dst) = makeTurn gs'
  putStrLn . concatMap printSq $ [src, dst]
  receiveInput $ move gs' a
handleInput [srcRow, srcCol, dstRow, dstCol] gs | all (`elem` ['A'..'G']) [srcRow, dstRow] && all (`elem` ['a'..'g']) [srcCol, dstCol] = do
  let gs' = move gs (Move (parseSq [srcRow, srcCol]) (parseSq [dstRow, dstCol]))
  receiveInput gs'
handleInput cmd gs = do
  hPutStrLn stderr $ "Illegal command: " ++ cmd
  receiveInput gs