module TicTacToe where

import Data.Either
import Data.Maybe
import Data.List

-- Board representation:                            =>    +---+---+---+
--                                                  =>    | X | 2 | 3 |
-- 1 Board consists of 3 rows                       =>    +---+---+---+
-- 1 Row consists of 3 Cells                        =>    | 4 | 5 | 6 |
-- 1 Cell contains either a Symbol or an Integer    =>    +---+---+---+
-- A symbol is either X (crosses) or O (noughts)    =>    | 7 | 8 | O |
-- Integers in cells represent cell address         =>    +---+---+---+

data Symbol = X | O
  deriving (Show, Eq)

type Cell = Either Symbol Int
type Row = [Cell]
type Col = [Cell]
type Diag = [Cell]

data Board = Board Row Row Row
  deriving (Eq)

instance Show Board where
  show board =
    (unlines (wrap "+---+---+---+" (map (concat . wrap "|" . map formatCell) (getBoardRows board))))
      where
        wrap :: String -> [String] -> [String]
        wrap pattern str = [pattern] ++ intersperse pattern str ++ [pattern]

        formatCell = either (\n -> " " ++ show n ++ " ") (\n -> "   ")

createEmptyBoard :: Board
createEmptyBoard = Board [Right 1, Right 2, Right 3] [Right 4, Right 5, Right 6] [Right 7, Right 8, Right 9]

getBoardRows :: Board -> [Row]
getBoardRows (Board row1 row2 row3) = [row1, row2, row3]

getBoardCols :: Board -> [Col]
getBoardCols (Board [a, b, c] [d, e, f] [g, h, i]) = [[a, d, g], [b, e, h], [c, f, i]]

getBoardDiags :: Board -> [Diag]
getBoardDiags (Board [a, b, c] [d, e, f] [g, h, i]) = [[a, e, i], [c, e, g]]

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

hasWinningRow :: [Row] -> Bool
hasWinningRow rows = or $ map allTheSame rows

hasWinningCol :: [Col] -> Bool
hasWinningCol cols = or $ map allTheSame cols

hasWinningDiag :: [Diag] -> Bool
hasWinningDiag diags = or $ map allTheSame diags

checkWin :: Board -> Bool
checkWin board = or $ [r, c, d]
  where
    r = hasWinningRow (getBoardRows board)
    c = hasWinningCol (getBoardCols board)
    d = hasWinningDiag (getBoardDiags board)

boardToList :: Board -> [Cell]
boardToList (Board row1 row2 row3) = row1 ++ row2 ++ row3

listToBoard :: [Cell] -> Board
listToBoard [c1, c2, c3, c4, c5, c6, c7, c8, c9] = Board [c1, c2, c3] [c4, c5, c6] [c7, c8, c9]

availableMoves :: Board -> [Cell]
availableMoves board = filter isRight (boardToList board)

checkDraw :: Board -> Bool
checkDraw board
  | checkWin board == True = False
  | length (availableMoves board) > 0 = False
  | otherwise = True

playMove :: Board -> Cell -> Cell -> Board
playMove board position move = resultBoard
  where
    boardList = boardToList board
    replaceBoard = map (\x -> if x == position then move else x) boardList
    resultBoard = listToBoard replaceBoard

getWinner :: Board -> String
getWinner board
  | checkDraw board == True = "It's a draw!"
  | hasPlayerWon board == True = "Congratulations! You win!"
  | otherwise = "The computer wins!"
    where
      hasPlayerWon :: Board -> Bool
      hasPlayerWon board = or allLines where
        allLines = map func rowsColsDiags
        func = (\x -> if (allTheSame x) && (head x) == (Left X) then True else False)
        rowsColsDiags = getBoardRows board

-- AI logic starts here
cellEmpty :: Cell -> Bool
cellEmpty cell = if cell == (Left O) || cell == (Left X) then False else True

canWin :: Board -> Cell -> Bool
canWin board symbol = or wins
  where
    wins = map (\x -> checkWin (playMove board x symbol)) (availableMoves board)

playWin :: Board -> Board
playWin board = playMove board (head winPositions) (Left O)
  where
    winPositions = filter (\x -> checkWin (playMove board x (Left O))) (availableMoves board)

canAILose :: Board -> Bool
canAILose board = canWin board (Left X)

playBlock :: Board -> Board
playBlock board = playMove board (head losePositions) (Left O)
  where
    losePositions = filter (\x -> checkWin (playMove board x (Left X))) (availableMoves board)

checkFork :: Board -> Cell -> Bool
checkFork board symbol = (length possibleWins) == 2
  where
    possibleWins = filter (\x -> checkWin (playMove board x symbol)) (availableMoves board)

canFork :: Board -> Cell -> Bool
canFork board symbol = or forks
  where
    forks = map (\x -> checkFork (playMove board x symbol) symbol) (availableMoves board)

playFork :: Board -> Board
playFork board = playMove board (head forkPositions) (Left O)
  where
    forkPositions = filter (\x -> checkFork (playMove board x (Left O)) (Left O)) (availableMoves board)

playBlockFork :: Board -> Board
playBlockFork board = playMove board (head forkPositions) (Left O)
  where
    forkPositions = filter (\x -> checkFork (playMove board x (Left X)) (Left X)) (availableMoves board)

isCenterEmpty :: Board -> Bool
isCenterEmpty board@(Board [_, _, _] [_, center, _] [_, _, _])
  | center == (Left O) || center == (Left X) = False
  | otherwise = True

playCenter :: Board -> Board
playCenter board@(Board [_, _, _] [_, center, _] [_, _, _]) = playMove board center (Left O)

canCorner :: Board -> Bool
canCorner board@(Board [topLeft, _, topRight] [_, _, _] [bottomLeft, _, bottomRight])
  | topLeft == (Left X) && isRight bottomRight = True
  | topRight == (Left X) && isRight bottomLeft = True
  | bottomLeft == (Left X) && isRight topRight = True
  | bottomRight == (Left X) && isRight topLeft = True
  | otherwise = False

playOppositeCorner :: Board -> Board
playOppositeCorner board@(Board [topLeft, _, topRight] [_, _, _] [bottomLeft, _, bottomRight])
  | topLeft == (Left X) && isRight bottomRight = playMove board bottomRight (Left O)
  | topRight == (Left X) && isRight bottomLeft = playMove board bottomLeft (Left O)
  | bottomLeft == (Left X) && isRight topRight = playMove board topRight (Left O)
  | bottomRight == (Left X) && isRight topLeft = playMove board topLeft (Left O)
  | otherwise = playFirstAvailable board

hasEmptyCorner :: Board -> Bool
hasEmptyCorner board@(Board [topLeft, _, topRight] [_, _, _] [bottomLeft, _, bottomRight])
  | isRight topLeft || isRight topRight || isRight bottomLeft || isRight bottomRight = True
  | otherwise = False

playEmptyCorner :: Board -> Board
playEmptyCorner board@(Board [topLeft, _, topRight] [_, _, _] [bottomLeft, _, bottomRight])
  | isRight topLeft = playMove board topLeft (Left O)
  | isRight topRight = playMove board topRight (Left O)
  | isRight bottomLeft = playMove board bottomLeft (Left O)
  | isRight bottomRight = playMove board bottomRight (Left O)
  | otherwise = playFirstAvailable board

hasEmptySide :: Board -> Bool
hasEmptySide board@(Board [_, topSide, _] [leftSide, _, rightSide] [_, bottomSide, _])
  | isRight topSide || isRight leftSide || isRight rightSide || isRight bottomSide = True
  | otherwise = False

playEmptySide :: Board -> Board
playEmptySide board@(Board [_, topSide, _] [leftSide, _, rightSide] [_, bottomSide, _])
  | isRight topSide = playMove board topSide (Left O)
  | isRight leftSide = playMove board leftSide (Left O)
  | isRight rightSide = playMove board rightSide (Left O)
  | isRight bottomSide = playMove board bottomSide (Left O)
  | otherwise = playFirstAvailable board

playFirstAvailable :: Board -> Board
playFirstAvailable board = playMove board (head (availableMoves board)) (Left O)

-- Follows the unbeatable strategy explained here https://en.wikipedia.org/wiki/Tic-tac-toe
--
bestMoveAI :: Board -> Board
bestMoveAI board
  | (canWin board (Left O)) == True = playWin board
  | (canAILose board) == True = playBlock board
  | (canFork board (Left O)) == True = playFork board
  | (canFork board (Left X)) == True = playEmptySide board
  | (isCenterEmpty board) == True = playCenter board
  | (canCorner board) == True = playOppositeCorner board
  | (hasEmptyCorner board) == True = playEmptyCorner board
  | (hasEmptySide board) == True = playEmptySide board
  | otherwise = playFirstAvailable board
