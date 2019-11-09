module Play where

  import Board (Position, Piece(White, Black, Empty), Board, width, height, positions)
  data Player = PlayerW | PlayerB deriving (Eq, Show)
  
  idx :: Position -> Int
  idx (x, y) = x * width + y

  checkValid :: Board -> Position -> Bool
  checkValid board pos
    | (snd (board !! (idx pos)) == Empty) && ((length (getAdjList board pos)) /= 0) = True
    | otherwise = False

  getAdjList :: Board -> Position -> [Position]
  getAdjList board (x, y) = [fst (board !! (idx (i, j))) | i <- [x-1..x+1], j <- [y-1..y+1], snd (board !! (idx (i, j))) /= Empty, (i, j) /= (x, y) ]

  move :: Board -> Position -> Player -> Board
  move board pos player
    | checkValid board pos =
      if player == PlayerW then replace board (pos, White)
      else replace board (pos, Black)
    | otherwise = board

  replace :: Board -> (Position, Piece) -> Board
  replace [] _ = []
  replace ((curPos, curPiece):rest) (pos, piece)
    | pos == curPos = (pos, piece):rest
    | otherwise = (curPos, curPiece):(replace rest (pos, piece))      