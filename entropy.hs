import Data.List (inits, tails, (\\))

-- dummy types, might be refined to gain type-safety and performance (custom enums + data.array?)
type Color = Char
type Pos = (Char, Char)
type Board = [(Pos, Color)]
type Score = Int

-- list of all valid positions in the game
positions :: [Pos]
positions = [(r, c) | r <- ['A' .. 'G'], c <- ['a' .. 'g']]

-- returns piece on position
piece :: Pos -> Board -> Maybe Color
piece = lookup

-- returns all empty positions
empty :: Board -> [Pos]
empty xs = positions \\ map fst xs

-- dummy implementation: don't move any piece (i.e. move piece onto itself, which is a legal move)
playOrderTurn :: Board -> (Pos, Pos)
playOrderTurn board = (pos, pos)
  where pos = fst . head $ board

-- dummy implementation: just take first free position
playChaosTurn :: Color -> Board -> Pos
playChaosTurn color board = head $ empty board

-- helper function to move a particular piece on the board
move :: Pos -> Pos -> Board -> Board
move src dst [] = []
move src dst ((pos, col) : bs) | pos == src = (dst, col) : bs
move src dst ((pos, col) : bs) = (pos, col) : bs

---- scoring related functions ----

continuousSubSeqs :: [a] -> [[a]]
continuousSubSeqs xs = [ts | is <- inits xs, ts <- tails is]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- simple (but inefficient) baseline score function for single pattern
patternScore :: Eq a => [a] -> Score
patternScore xs = sum $ filter (> 1) $ map length $ filter isPalindrome (continuousSubSeqs xs)

---- below this line are all messy IO functions ----

main :: IO ()
main = receiveInput []

receiveInput :: Board -> IO ()
receiveInput board = do
  cmd <- getLine
  handleInput cmd board

handleInput :: String -> Board -> IO ()
handleInput "Start" board = receiveInput board
handleInput "Quit" board = return ()
handleInput [color] board = do
  let pos@(row, col) = playChaosTurn color board
  putStrLn [row, col]
  receiveInput $ (pos, color) : board
handleInput [color, row, col] board = do
  let newBoard = ((row, col), color):board
  let (src@(srcRow, srcCol), dst@(dstRow, dstCol)) = playOrderTurn newBoard
  putStrLn [srcRow, srcCol, dstRow, dstCol]
  receiveInput $ move src dst board
handleInput [srcRow, srcCol, dstRow, dstCol] board = 
  receiveInput $ move (srcRow, srcCol) (dstRow, dstCol) board
handleInput cmd board = errorWithoutStackTrace $ "Illegal command: " ++ cmd