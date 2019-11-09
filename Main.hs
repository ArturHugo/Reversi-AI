import Board
import Play

main = do
  printBoard emptyBoard
  pos' <- getLine
  let pos = read pos' :: Position
  printBoard (move emptyBoard pos PlayerW)

