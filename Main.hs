import Board
import Play

gameRound :: Board -> Player -> IO()
gameRound board player =
  if gameOver board positions player then
    if countScore board PlayerW > countScore board PlayerB then
      putStr "Game over\nWhite pieces win\n"
    else
      if countScore board PlayerB > countScore board PlayerW then
        putStr "Game over\nBlack pieces win\n"
      else
        putStr "Game over\nIt's a tie\n"
  else do
    pos' <- getLine
    let pos = read pos' :: Position
    let newBoard = move board pos player
    printBoard (newBoard)
    if newBoard /= board then
      if player == PlayerW then
        gameRound newBoard PlayerB
      else
        gameRound newBoard PlayerW
    else
      gameRound newBoard player

main = do
  printBoard emptyBoard
  gameRound emptyBoard PlayerW

