module Board where

  import Data.List

  data Piece = White | Black | Empty deriving (Eq, Show)
  showPiece :: Piece -> String
  showPiece piece
    | piece == White = "O"
    | piece == Black = "X"
    | otherwise = " "
  type Position = (Int, Int)
  type Board = [(Position, Piece)]
  width = 4
  height = 4

  positions = [(x, y) | x <- [0..height-1], y <- [0..width-1]]
  emptyBoard = [initBoard pos | pos <- positions]
  initBoard :: Position -> (Position, Piece)
  initBoard pos
    | pos == (width `div` 2 - 1, height `div` 2 - 1) || pos == (width `div` 2, height `div` 2) = (pos, White)
    | pos == (width `div` 2 - 1, height `div` 2) || pos == (width `div` 2, height `div` 2 - 1) = (pos,Black)
    | otherwise = (pos, Empty)

  lineSeparator :: Int -> String
  lineSeparator 0 = " ---\n"
  lineSeparator x = " ---" ++ lineSeparator (x-1)

  revRowIdxs :: Int -> String
  revRowIdxs 0 = "  " ++ (show 0) ++ "  "
  revRowIdxs x = "  " ++ (show x) ++ " " ++ revRowIdxs (x-1)

  rowIdxs :: Int -> String
  rowIdxs x = reverse (revRowIdxs x) ++ "\n"

  boardToStr :: Board -> String
  boardToStr [] = ""
  boardToStr [(lastPos, lastPiece)] = " " ++ (showPiece lastPiece) ++ "\n" ++ (rowIdxs (snd lastPos))
  boardToStr ((curPos, curPiece):rest)
    | snd curPos == 0 = show (fst curPos) ++ restOfLine
    | otherwise = restOfLine
    where
      restOfLine
        | snd (fst (head rest)) == 0 = " " ++ (showPiece curPiece) ++ " \n" ++ (lineSeparator (snd curPos)) ++ restStr
        | otherwise = " " ++ (showPiece curPiece) ++ " |" ++ restStr
        where
          restStr = (boardToStr rest)

  printBoard :: Board -> IO()
  printBoard board = putStr (boardToStr board)