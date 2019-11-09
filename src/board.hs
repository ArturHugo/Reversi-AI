import Data.List
import Data.Maybe
import qualified Data.Map

data Piece = White | Black | Empty deriving (Eq, Show)
showPiece :: Piece -> String
showPiece piece
  | piece == White = "O"
  | piece == Black = "X"
  | otherwise = " "
type Position = (Integer, Integer)
type Board = [(Position, Piece)]

positions = [(x, y) | x <- [0..7], y <- [0..7]]
emptyBoard = [(pos, piece) | pos <- positions, piece <- [Empty]]

lineSeparator :: Integer -> String
lineSeparator 0 = " ---\n"
lineSeparator x = " ---" ++ lineSeparator (x-1)

revTopRow :: Int -> String
revTopRow 0 = "  " ++ (show 0) ++ "  "
revTopRow x = "  " ++ (show x) ++ " " ++ revTopRow (x-1)

topRow :: Int -> String
topRow x = reverse (revTopRow x) ++ "\n"

boardToStr :: Board -> String
boardToStr [] = ""
boardToStr [(lastPos, lastPiece)] = (showPiece lastPiece) ++ "\n"
boardToStr all@((curPos, curPiece):rest)
  | snd curPos == 0 = show (fst curPos) ++ restOfLine
  | otherwise = restOfLine
  where
    restOfLine
      | snd (fst (head rest)) == 0 = " " ++ (showPiece curPiece) ++ " \n" ++ (lineSeparator (snd curPos)) ++ restStr
      | otherwise = " " ++ (showPiece curPiece) ++ " |" ++ restStr
      where
        restStr = (boardToStr rest)