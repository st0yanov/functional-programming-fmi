module Main where

import TicTacToe

main :: IO ()
main = do
  putStrLn "Tic-Tac-Toe"
  putStrLn "Haskell Project @ FMI"
  putStrLn "-----------------------------"

  gameLoop createEmptyBoard

gameEnd :: Board -> IO ()
gameEnd board = do
  putStrLn (show board)
  putStrLn $ getWinner board

gameLoop :: Board -> IO ()
gameLoop board = do
  putStrLn (show board)

  putStrLn "Enter cell address (1-9) to place your X: "
  move <- getLine
  let newBoard = playMove board (Right $ read move) (Left X)

  if newBoard == board
    then gameLoop board
    else
      if or [checkWin newBoard, checkDraw newBoard]
        then gameEnd newBoard
        else gameAI newBoard

gameAI :: Board -> IO ()
gameAI board = do
  let newBoard = bestMoveAI board
  if or [checkWin newBoard, checkDraw newBoard]
    then gameEnd newBoard
    else gameLoop newBoard
