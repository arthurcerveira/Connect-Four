module Main where

-- cabal install ansi-terminal
import System.IO
import System.Console.ANSI

-- Board displayed to the user
type GameBoard = [[Char]]

-- Control board
-- 0: Empty
-- 1: Player 1
-- 2: Player 2
type ControlBoard = [[Int]]

playerOne :: Int
playerOne = 1

playerTwo :: Int
playerTwo = 2

gameBoard :: GameBoard
gameBoard = [['-','-','-','-','-','-','-'],
             ['-','-','-','-','-','-','-'],
             ['-','-','-','-','-','-','-'],
             ['-','-','-','-','-','-','-'],
             ['-','-','-','-','-','-','-'],
             ['-','-','-','-','-','-','-']]

controlBoard :: ControlBoard
controlBoard = [[0, 0, 1, 1, 2, 1, 0],
                [0, 0, 1, 1, 0, 0, 0],
                [0, 0, 2, 2, 1, 2, 0],
                [1, 0, 0, 0, 2, 2, 0],
                [0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0]] 

rowsArray :: [Int]
rowsArray = [0, 1, 2, 3, 4, 5]

columnsArray :: [Int]
columnsArray = [0, 1, 2, 3, 4, 5, 6]

-- Function to manage the board
getArray :: Int -> [t] -> t
getArray index row = row !! index

updateArray :: Int -> t -> [t] -> [t]
updateArray index element (row:rows)
   | index > 0 = row : updateArray (index - 1) element rows
   | otherwise = element : rows

getPosition :: Int -> Int -> [[a]] -> a
getPosition row column board = getArray column (getArray row board)

updatePosition :: Int -> Int -> a -> [[a]] -> [[a]]
updatePosition row column element board = updateArray row (updateArray column element (getArray row board)) board

-- Functions to insert pieces into the board
isEmpty :: Int -> Int -> ControlBoard -> Bool
isEmpty row column board = (getPosition row column board) == 0

findEmptyRow :: Int -> ControlBoard -> Int
findEmptyRow column board = findEmptyRowAux 0 column board
  where
    findEmptyRowAux :: Int -> Int -> ControlBoard -> Int
    findEmptyRowAux row column board
      | (isEmpty row column board) = row
      | otherwise = findEmptyRowAux (row + 1) column board

isValidPosition :: Int -> Int -> Bool
isValidPosition row column = (row < 6) && (column < 7) && (row >= 0) && (column >= 0)

isValidMove :: Int -> ControlBoard -> Bool -- Collumn cannot be filled to the top
isValidMove column board
  | (column < 7) && (column >= 0) = isEmpty 5 column board
  | otherwise = False

play :: Int -> Int -> ControlBoard -> ControlBoard
play player column board
  | isValidMove column board = updatePosition (findEmptyRow column board) column player board
  | otherwise = board

-- Functions to verify if the player won
winMoves :: [[(Int, Int)]]
winMoves = [[(0, 0), (0, 1),  (0, 2),  (0, 3)],
            [(0, 0), (1, 0),  (2, 0),  (3, 0)],
            [(0, 0), (1, 1),  (2, 2),  (3, 3)],
            [(0, 0), (-1, 1), (-2, 2), (-3, 3)]]

verifyMove :: Int -> Int -> Int -> ControlBoard -> [(Int, Int)] -> Bool
verifyMove row column player board [] = True  -- If all move positions == player, player won
verifyMove row column player board (move:moves)
  | not (isValidPosition (row + (fst move)) (column + (snd move))) = False
  | getPosition (row + (fst move)) (column + (snd move)) board == player = verifyMove row column player board moves
  | otherwise = False

verifyPosition :: Int -> Int -> Int -> ControlBoard -> [[(Int, Int)]] -> Bool
verifyPosition row column player board [] = False  -- If all moves == False, player did not win
verifyPosition row column player board (move:moves)
  | player /= getPosition row column board = False  -- Verify if player is in the position
  | verifyMove row column player board move = True
  | otherwise = verifyPosition row column player board moves

didPlayerWon :: Int -> ControlBoard -> Bool
didPlayerWon player board = didPlayerWonRecur player board rowsArray columnsArray
  where
      didPlayerWonRecur :: Int -> ControlBoard -> [Int] -> [Int] -> Bool  -- Iterate through the board
      didPlayerWonRecur player board [] columns = False  
      didPlayerWonRecur player board (row:rows) [] = didPlayerWonRecur player board rows columnsArray  
      didPlayerWonRecur player board (row:rows) (column:columns)  
        | verifyPosition row column player board winMoves = True
        | otherwise = didPlayerWonRecur player board (row:rows) columns

isDraw :: ControlBoard -> Bool
isDraw [] = True
isDraw (row:rows) = isDrawRecur row && isDraw rows
  where
      isDrawRecur :: [Int] -> Bool
      isDrawRecur [] = True
      isDrawRecur (column:columns)
        | column == 0 = False
        | otherwise = True && isDrawRecur columns


main = do
    -- setCursorPosition 5 0
    -- setTitle "ANSI Terminal Short Example"

    setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid Red
           ]
    putStr "Hello "

    setSGR [ SetConsoleIntensity NormalIntensity
           , SetColor Foreground Vivid White
           , SetColor Background Dull Blue
           ]
    putStr "World!"
    setSGR [Reset]
    putStr "\n"