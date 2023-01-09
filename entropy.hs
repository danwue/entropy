#!/usr/bin/env runhaskell

import Data.Array (Array, assocs, elems, indices, listArray, (!), (//))
import Data.Function (on)
import Data.Ix (index)
import Data.List (inits, maximumBy, minimumBy, tails)
import Data.Maybe (catMaybes, isJust, isNothing)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

-- Types --

type Square = (Int, Int)
data Counter = Blue | Yellow | Red | Green | Pink | Maroon | Black deriving (Enum, Eq, Ord)
type Board = Array Square (Maybe Counter)
data Action = Pick Counter | Place Square | Move Square Square
data GameState = AwaitingRandom Board | AwaitingChaos Counter Board | AwaitingOrder Board

-- Constants --

boardDimension :: Int
boardDimension = 7

emptyBoard :: Array Square (Maybe Counter)
emptyBoard = listArray ((1, 1), (boardDimension, boardDimension)) $ replicate (boardDimension * boardDimension) Nothing

initialState :: GameState
initialState = AwaitingRandom emptyBoard

-- Functions related to querying and modifying game states --

apply :: GameState -> Action -> GameState
apply (AwaitingChaos counter board) (Place pos) = AwaitingOrder $ board // [(pos, Just counter)]
apply (AwaitingOrder board) (Move src dst) = AwaitingRandom $ board // [(src, Nothing), (dst, board ! src)]
apply (AwaitingRandom board) (Pick color) = AwaitingChaos color board
apply action state = error "Illegal action for given game state"

remainingCounters :: Counter -> Board -> Int
remainingCounters counter = (boardDimension -) . length . filter (== counter) . catMaybes . elems

emptySquares :: Board -> [Square]
emptySquares = map fst . filter (isNothing . snd) . assocs

occupiedSquares :: Board -> [Square]
occupiedSquares = map fst . filter (isJust . snd) . assocs

leftOf :: Square -> [Square]
leftOf (r, c) = reverse [(r, c) | c' <- [1 .. c -1]]

rightOf :: Square -> [Square]
rightOf (r, c) = [(r, c) | c' <- [c + 1 .. boardDimension]]

upOf :: Square -> [Square]
upOf (r, c) = reverse [(r, c) | r' <- [1 .. r -1]]

downOf :: Square -> [Square]
downOf (r, c) = [(r, c) | r' <- [r + 1 .. boardDimension]]

validMoves :: Square -> Board -> [Square]
validMoves sq board | isNothing $ board ! sq = []
validMoves sq board = concatMap (takeWhile (\sq' -> isNothing $ board ! sq')) [leftOf sq, rightOf sq, upOf sq, downOf sq]

skipMove :: Board -> Action
skipMove board = Move occupiedSquare occupiedSquare
  where
    occupiedSquare = head . occupiedSquares $ board

actions :: GameState -> [Action]
actions (AwaitingRandom board) = [Pick counter | counter <- [Blue .. Black], remainingCounters counter board > 0]
actions (AwaitingChaos counter board) = map Place $ emptySquares board
actions (AwaitingOrder board) = skipMove board : [Move src dst | src <- indices board, dst <- validMoves src board]

board :: GameState -> Board
board (AwaitingRandom board) = board
board (AwaitingChaos _ board) = board
board (AwaitingOrder board) = board

rows :: Board -> [[Maybe Counter]]
rows board = [[board ! (r, c) | c <- [1 .. boardDimension]] | r <- [1 .. boardDimension]]

cols :: Board -> [[Maybe Counter]]
cols board = [[board ! (r, c) | r <- [1 .. boardDimension]] | c <- [1 .. boardDimension]]

continuousSubSeqs :: [a] -> [[a]]
continuousSubSeqs xs = [ts | is <- inits xs, ts <- tails is]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

patternScore :: Eq a => [a] -> Int
patternScore xs = sum $ filter (> 1) $ map length $ filter isPalindrome (continuousSubSeqs xs)

-- Main implementation of the algorithm --

minimumOn :: Ord a => (c -> a) -> [c] -> c
minimumOn f = snd . minimumBy (compare `on` fst) . map (\x -> (f x, x))

maximumOn :: Ord a => (c -> a) -> [c] -> c
maximumOn f = snd . maximumBy (compare `on` fst) . map (\x -> (f x, x))

makeTurn :: GameState -> Action
makeTurn gs@(AwaitingChaos _ _) = minimumOn (score 4 . apply gs) $ actions gs
makeTurn gs@(AwaitingOrder _) = maximumOn (score 4 . apply gs) $ actions gs
makeTurn _ = error "Illegal state"

score :: Int -> GameState -> Int
score 0 gs = heuristic . board $ gs
score d gs | null . emptySquares . board $ gs = heuristic . board $ gs
score d gs@(AwaitingRandom board) = sum [remainingCounters counter board * score (d -1) (apply gs a) | a@(Pick counter) <- actions gs] `div` (length . emptySquares $ board)
score d gs@(AwaitingChaos _ _) = minimum . map (score (d -1) . apply gs) $ actions gs
score d gs@(AwaitingOrder _) = maximum . map (score (d -1) . apply gs) $ actions gs

heuristic :: Board -> Int
heuristic board = sum . map patternScore $ rows board ++ cols board

-- Communication protocol and all IO stuff --

class Protocol a where
  parse :: String -> [a]
  print :: a -> String

parseCounter :: String -> Counter
parseCounter = toEnum . (+ (-1)) . read

parseSq :: String -> Square
parseSq [r, c] = (index ('A', 'Z') r + 1, index ('a', 'z') c + 1)
parseSq s = error $ "Illegal square identifier: " ++ s

printSq :: Square -> String
printSq (r, c) = [['A' .. 'Z'] !! (r -1), ['a' .. 'z'] !! (c -1)]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  receiveInput initialState

receiveInput :: GameState -> IO ()
receiveInput gs = do
  cmd <- getLine
  handleInput cmd gs

handleInput :: String -> GameState -> IO ()
handleInput "Start" gs = receiveInput gs
handleInput "Quit" gs = return ()
handleInput [counter] gs = do
  let gs' = apply gs (Pick (parseCounter [counter]))
  let a@(Place sq) = makeTurn gs'
  putStrLn . printSq $ sq
  receiveInput $ apply gs' a
handleInput [counter, row, col] gs = do
  let gs' = apply gs (Pick (parseCounter [counter]))
  let gs'' = apply gs' (Place (parseSq [row, col]))
  let a@(Move src dst) = makeTurn gs''
  putStrLn . concatMap printSq $ [src, dst]
  receiveInput $ apply gs'' a
handleInput [srcRow, srcCol, dstRow, dstCol] gs = do
  let gs' = apply gs (Move (parseSq [srcRow, srcCol]) (parseSq [dstRow, dstCol]))
  receiveInput gs'
handleInput cmd board = errorWithoutStackTrace $ "Illegal command: " ++ cmd