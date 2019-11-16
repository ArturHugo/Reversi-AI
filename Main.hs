import Board
import Play
import AI

gameRound :: Board -> Player -> Bool -> IO()
gameRound board player isAI =
  if gameOver board positions player then
    if countScore board PlayerW > countScore board PlayerB then
      putStr "Game over\nWhite pieces win\n"
    else
      if countScore board PlayerB > countScore board PlayerW then
        putStr "Game over\nBlack pieces win\n"
      else
        putStr "Game over\nIt's a tie\n"
  else
    if isAI && (player == PlayerB) then do
      let pos = fst (minimax board positions player True 0 3)
      let newBoard = move board pos player
      printBoard newBoard
      if newBoard /= board then
        if player == PlayerW then
          gameRound newBoard PlayerB isAI
        else
          gameRound newBoard PlayerW isAI
      else
        gameRound newBoard player isAI
    else do
      pos' <- getLine
      let pos = read pos' :: Position
      let newBoard = move board pos player
      printBoard newBoard
      if newBoard /= board then
        if player == PlayerW then
          gameRound newBoard PlayerB isAI
        else
          gameRound newBoard PlayerW isAI
      else
        gameRound newBoard player isAI

main = do
  printBoard emptyBoard
  gameRound emptyBoard PlayerW True

