module Main where

import System.IO 
import Control.Monad  -- To use 'when'
-- cabal install ansi-terminal
import System.Console.ANSI  -- Add color to output

-- Control board
-- 0: Empty
-- 1: Player 1
-- 2: Player 2
type ControlBoard = [[Int]]

playerOne :: Int
playerOne = 1

playerTwo :: Int
playerTwo = 2

controlBoard :: ControlBoard
controlBoard = [[0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0]]

columnsMap :: [(String, Int)]
columnsMap = [("a", 0), ("b", 1), ("c", 2),("d", 3), 
              ("e", 4), ("f", 5),("g", 6)]

rowsArray :: [Int]
rowsArray = [0, 1, 2, 3, 4, 5]

columnsArray :: [Int]
columnsArray = [0, 1, 2, 3, 4, 5, 6]

inverseRowsArray :: [Int]
inverseRowsArray = [5, 4, 3, 2, 1, 0]

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
winMoves = [[(0, 0), (0, 1),  (0, 2),  (0, 3)],  -- Horizontal
            [(0, 0), (1, 0),  (2, 0),  (3, 0)],  -- Vertical
            [(0, 0), (1, 1),  (2, 2),  (3, 3)],  -- Diagonal
            [(0, 0), (-1, 1), (-2, 2), (-3, 3)]] -- Diagonal

-- Verify a wining move for a position
verifyMove :: Int -> Int -> Int -> ControlBoard -> [(Int, Int)] -> Bool
verifyMove row column player board [] = True  -- If all move positions == player, player won
verifyMove row column player board (move:moves)
  | not (isValidPosition (row + (fst move)) (column + (snd move))) = False
  | getPosition (row + (fst move)) (column + (snd move)) board == player = verifyMove row column player board moves
  | otherwise = False

-- Verify every wining move for a position
verifyPosition :: Int -> Int -> Int -> ControlBoard -> [[(Int, Int)]] -> Bool
verifyPosition row column player board [] = False  -- If all moves == False, player did not win
verifyPosition row column player board (move:moves)
  | player /= getPosition row column board = False  -- Verify if player is in the position
  | verifyMove row column player board move = True
  | otherwise = verifyPosition row column player board moves

-- Loop through the board and verify every position
didPlayerWon :: Int -> ControlBoard -> Bool
didPlayerWon player board = didPlayerWonRecur player board rowsArray columnsArray
  where
      didPlayerWonRecur :: Int -> ControlBoard -> [Int] -> [Int] -> Bool  -- Iterate through the board
      didPlayerWonRecur player board [] columns = False  
      didPlayerWonRecur player board (row:rows) [] = didPlayerWonRecur player board rows columnsArray  
      didPlayerWonRecur player board (row:rows) (column:columns)  
        | verifyPosition row column player board winMoves = True
        | otherwise = didPlayerWonRecur player board (row:rows) columns

-- If every position is filled and none of the players won, the game is a draw
isDraw :: ControlBoard -> Bool
isDraw [] = True
isDraw (row:rows) = isDrawRecur row && isDraw rows
  where
      isDrawRecur :: [Int] -> Bool
      isDrawRecur [] = True
      isDrawRecur (column:columns)
        | column == 0 = False
        | otherwise = True && isDrawRecur columns

-- Get wining positions
getWinPositions :: ControlBoard -> [(Int, Int)]
getWinPositions board = []

-- Functions to manage IO operations
resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

printInstructionMessage :: IO ()
printInstructionMessage = do
  resetScreen
  setSGR [SetConsoleIntensity BoldIntensity]
  putStr "\n Bem vindo ao jogo Connect Four!\n Digite um caractere de 'a' à 'g':\n"
  setSGR [Reset]

isWinPosition :: Int -> Int -> [(Int, Int)] -> Bool
isWinPosition row column [] = False
isWinPosition row column (winPosition:winPositions)
    | row == (fst winPosition) && column == (snd winPosition) = True
    | otherwise = isWinPosition row column winPositions

printBoard :: ControlBoard -> [Int] -> [Int] -> [(Int, Int)] -> IO ()
printBoard board [] columns winPositions = putStr ""  
printBoard board (row:rows) [] winPositions = do 
  putStr "\n  "  
  printBoard board rows columnsArray winPositions
printBoard board (row:rows) (column:columns) winPositions = do
  let position = getPosition row column board

  -- Player 1 is color red
  when (position == playerOne) $ setSGR [SetColor Foreground Vivid Red]
  -- Player 2 is color blue
  when (position == playerTwo) $ setSGR [SetColor Foreground Vivid Blue]
  -- Set win positions bold
  when (isWinPosition row column winPositions) $  setSGR [SetConsoleIntensity BoldIntensity]

  putStr ("O ")
  setSGR [Reset]

  printBoard board (row:rows) columns winPositions

printHeader :: IO ()
printHeader = do
  setSGR [SetConsoleIntensity BoldIntensity]
  putStr "\n  a b c d e f g\n  "
  setSGR [Reset]

printPlayer :: Int -> IO ()
printPlayer player = do
  putStr "\n Vez do "

  when (player == playerOne) $ setSGR [SetColor Foreground Vivid Red]
  when (player == playerTwo) $ setSGR [SetColor Foreground Vivid Blue]

  putStr ("Jogador " ++ (show player) ++ ": ")

  setSGR [Reset]

printPlayerWon :: Int -> IO ()
printPlayerWon player = do
  when (player == playerOne) $ setSGR [SetColor Foreground Vivid Red]
  when (player == playerTwo) $ setSGR [SetColor Foreground Vivid Blue]

  putStr ("\n Jogador " ++ (show player))

  setSGR [Reset]

  putStr " ganhou!\n\n"

getColumn :: String -> [(String, Int)] -> Int
getColumn letter [] = -1
getColumn letter (element:table)
  | letter == fst element = snd element
  | otherwise = getColumn letter table

main = do
  printInstructionMessage
  printHeader
  printBoard controlBoard inverseRowsArray columnsArray []
  gameLoop controlBoard playerOne

gameLoop :: ControlBoard -> Int -> IO ()
gameLoop board player = do
  printPlayer player
  columnKey <- getLine

  let column = getColumn columnKey columnsMap

  if (column == -1) then do gameLoop board player -- Validate input
  else do
    let newBoard = play player column board    
    -- Verify if player won
    if (didPlayerWon player newBoard) then do 
      let winPositions = getWinPositions newBoard

      printInstructionMessage

      printHeader
      printBoard newBoard inverseRowsArray columnsArray winPositions
    
      printPlayerWon player
    else do
      -- Verify if the game was a draw
      if (isDraw newBoard) then do putStr " Empate!\n\n"
      else do
        printInstructionMessage

        printHeader
        printBoard newBoard inverseRowsArray columnsArray []

        -- Next round
        when (player == playerOne) $ gameLoop newBoard playerTwo
        when (player == playerTwo) $ gameLoop newBoard playerOne
