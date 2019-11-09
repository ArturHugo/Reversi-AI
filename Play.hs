module Play where

  import Board (Position, Piece(White, Black, Empty), Board, width, height, positions)
  data Player = PlayerW | PlayerB deriving (Eq, Show)
  
  idx :: Position -> Int
  idx pos = (fst pos) * width + (snd pos)

  checkValid :: Board -> Position -> Bool
  checkValid board pos
    | snd (board !! (idx pos)) == Empty = True
    | otherwise = False

--  getAdjList :: Board -> 

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