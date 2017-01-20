module TicTacToe where

import Data.Maybe
import Data.Either
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
        wrap with str = [with] ++ intersperse with str ++ [with]

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
listToBoard [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10] = Board [c1, c2, c3] [c4, c5, c6] [c7, c8, c9]

availableMoves :: Board -> [Cell]
availableMoves board = filter isRight (boardToList board)

checkDraw :: Board -> Bool
checkDraw board
  | checkWin board == True = False
  | length (availableMoves board) > 0 = False
  | otherwise = True

