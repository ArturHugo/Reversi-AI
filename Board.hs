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
  width = 7
  height = 7

  positions = [(x, y) | x <- [0..width], y <- [0..height]]
  emptyBoard = [initBoard pos | pos <- positions]
  initBoard :: Position -> (Position, Piece)
  initBoard pos
    | pos == (3,3) || pos == (4,4) = (pos, White)
    | pos == (3,4) || pos == (4,3) = (pos,Black)
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
  boardToStr [(lastPos, lastPiece)] = (showPiece lastPiece) ++ "\n" ++ (rowIdxs (snd lastPos))
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