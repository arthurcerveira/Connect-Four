module Main where

import System.IO

-- Board displayed to the user
type GameBoard = [[Char]]

-- Control board
-- 0: empty
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
controlBoard = [[0, 0, 0, 0, 0, 0, 0],
                [0, 4, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0]]

-- Function to manage the board
getArray :: Int -> [t] -> t
getArray index line = line !! index

updateArray :: Int -> t -> [t] -> [t]
updateArray index element (line:lines)
   | index > 0 = line : updateArray (index - 1) element lines
   | otherwise = element : lines

getPosition :: Int -> Int -> [[a]] -> a
getPosition line column board = getArray column (getArray line board)

updatePosition :: Int -> Int -> a -> [[a]] -> [[a]]
updatePosition line column element board = updateArray line (updateArray column element (getArray line board)) board


main :: IO ()
main = print "Hello World!"