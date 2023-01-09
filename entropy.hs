#!/usr/bin/env runhaskell

import Data.List (inits, tails, sortOn)
import Data.Array (Array, assocs, elems, indices, listArray, (!), (//))
import Data.Ix (index)
import Data.Maybe (catMaybes, isJust, isNothing)

-- Types --

type Square = (Int, Int)
data Counter = Blue | Yellow | Red | Green | Pink | Maroon | Black deriving (Enum, Eq)
type Board = Array Square (Maybe Counter)
data Action = Pick Counter | Place Square | Move Square Square
data GameState = AwaitingRandom Board | AwaitingChaos Counter Board | AwaitingOrder Board

-- Constants --

boardDimension :: Int
boardDimension = 7

emptyBoard :: Array Square (Maybe Counter)
emptyBoard = listArray ((0, 0), (boardDimension -1, boardDimension -1)) $ replicate (boardDimension * boardDimension) Nothing

initialState :: GameState
initialState = AwaitingRandom emptyBoard

-- Functions related to querying and modifying game states --

apply :: Action -> GameState -> GameState
apply (Place pos) (AwaitingChaos counter board) = AwaitingOrder $ board // [(pos, Just counter)]
apply (Move src dst) (AwaitingOrder board) = AwaitingRandom $ board // [(src, Nothing), (dst, board ! src)]
apply (Pick color) (AwaitingRandom board) = AwaitingChaos color board
apply action state = error "Illegal action for given game state"

remainingCounters :: Counter -> Board -> Int
remainingCounters counter = (boardDimension -) . length . filter (== counter) . catMaybes . elems

emptySquares :: Board -> [Square]
emptySquares = map fst . filter (isNothing . snd) . assocs

leftOf :: Square -> [Square]
leftOf (r, c) = reverse [(r, c) | c' <- [0 .. c -1]]

rightOf :: Square -> [Square]
rightOf (r, c) = [(r, c) | c' <- [c + 1 .. boardDimension -1]]

upOf :: Square -> [Square]
upOf (r, c) = reverse [(r, c) | r' <- [0 .. r -1]]

downOf :: Square -> [Square]
downOf (r, c) = [(r, c) | r' <- [r + 1 .. boardDimension -1]]

validMoves :: Square -> Board -> [Square]
validMoves sq board | isNothing $ board ! sq = []
validMoves sq board = concatMap (takeWhile (\sq' -> isNothing $ board ! sq')) [leftOf sq, rightOf sq, upOf sq, downOf sq]

skipMove :: Board -> Action
skipMove board = Move occupiedSquare occupiedSquare
  where
    occupiedSquare = fst . head . filter (isJust . snd) . assocs $ board

actions :: GameState -> [Action]
actions (AwaitingRandom board) = [Pick counter | counter <- [Blue .. Black], remainingCounters counter board > 0]
actions (AwaitingChaos counter board) = map Place $ emptySquares board
actions (AwaitingOrder board) = skipMove board : [Move src dst | src <- indices board, dst <- validMoves src board]

board :: GameState -> Board
board (AwaitingRandom board) = board
board (AwaitingChaos _ board) = board
board (AwaitingOrder board) = board

rows :: Board -> [[Maybe Counter]]
rows board  = [ [ board ! (r,c) | c <- [0..boardDimension-1] ] | r <- [0..boardDimension-1] ]

cols :: Board -> [[Maybe Counter]]
cols board  = [ [ board ! (r,c) | r <- [0..boardDimension-1] ] | c <- [0..boardDimension-1] ]

split :: [Maybe a] -> [[a]]
split [] = []
split (Nothing:xs) = split xs
split xs = catMaybes group : split remaining
  where (group, remaining) = span isJust xs

continuousSubSeqs :: [a] -> [[a]]
continuousSubSeqs xs = [ts | is <- inits xs, ts <- tails is]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

patternScore :: Eq a => [a] -> Int
patternScore xs = sum $ filter (> 1) $ map length $ filter isPalindrome (continuousSubSeqs xs)

boardScore :: Board -> Int
boardScore board = sum . map patternScore . concatMap split $ (rows board ++ cols board)

-- Main implementation of the algorithm --

-- Currently, we just choose the action which maximizes/minimizes the next game state.
makeTurn :: GameState -> Action
makeTurn gs@(AwaitingChaos _ _) = head $ sortOn (\a -> boardScore . board . apply a $ gs) $ actions gs
makeTurn gs@(AwaitingOrder _) = last $ sortOn (\a -> boardScore . board . apply a $ gs) $ actions gs
makeTurn _ = error "Illegal state"

-- Communication protocol and all IO stuff --

class Protocol a where
  parse :: String -> [a]
  print :: a -> String

parseCounter :: String -> Counter
parseCounter = toEnum . (+(-1)) . read

parseSq :: String -> Square
parseSq [r, c] = (index ('A', 'Z') r, index ('a', 'z') c)
parseSq s = error $ "Illegal square identifier: " ++ s

printSq :: Square -> String
printSq (r, c) = [['A' .. 'Z'] !! r, ['a' .. 'z'] !! c]

main :: IO ()
main = receiveInput initialState

receiveInput :: GameState -> IO ()
receiveInput gs = do
  cmd <- getLine
  handleInput cmd gs

handleInput :: String -> GameState -> IO ()
handleInput "Start" gs = receiveInput gs
handleInput "Quit" gs = return ()
handleInput [counter] gs = do
  let gs' = apply (Pick (parseCounter [counter])) gs
  let a@(Place sq) = makeTurn gs'
  putStrLn . printSq $ sq
  receiveInput $ apply a gs'
handleInput [counter, row, col] gs = do
  let gs' = apply (Pick (parseCounter [counter])) gs
  let gs'' = apply (Place (parseSq [row, col])) gs'
  let a@(Move src dst) = makeTurn gs''
  putStrLn . concatMap printSq $ [src, dst]
  receiveInput $ apply a gs''
handleInput [srcRow, srcCol, dstRow, dstCol] gs = do
  let gs' = apply (Move (parseSq [srcRow, srcCol]) (parseSq [dstRow, dstCol])) gs
  receiveInput gs'
handleInput cmd board = errorWithoutStackTrace $ "Illegal command: " ++ cmd