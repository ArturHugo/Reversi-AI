module Play where

  import Board (Position, Piece(White, Black, Empty), Board, width, height, positions)
  data Player = PlayerW | PlayerB deriving (Eq, Show)
  data Direction = N | NE | E | SE | S | SW | W | NW deriving (Eq, Show) 

  countScore :: Board -> Player -> Int
  countScore board player =
    (length . filter(== piece)) [snd (board !! x)| x <- [0..(length board)-1]]
    where
      piece
        | player == PlayerW = White
        | otherwise = Black

  gameOver :: Board -> [Position] -> Player -> Bool
  gameOver _ [] _ = True
  gameOver board unchecked player
    | checkValid board (head unchecked) player =
      False
    | otherwise = gameOver board (tail unchecked) player

  idx :: Position -> Int
  idx (x, y) = x * width + y

  checkValid :: Board -> Position -> Player -> Bool
  checkValid board pos player
    | (snd (board !! (idx pos)) == Empty) && (((getAllFlipLists board pos player)) /= []) = True
    | otherwise = False

  moveDir :: Position -> Direction -> Position
  moveDir (x, y) dir
    | dir == N && x > 0 = (x-1, y) 
    | dir == NE && x > 0 && y < width-1 = (x-1, y+1)
    | dir == E && y < width-1 = (x, y+1)
    | dir == SE && x < height-1 && y < width-1 = (x+1, y+1)
    | dir == S && x < height-1 = (x+1, y)
    | dir == SW && x < height-1 && y > 0 = (x+1, y-1)
    | dir == W && y > 0 = (x, y-1)
    | dir == NW && x > 0 && y > 0 = (x-1, y-1)
    | otherwise = (x, y)

  getFlipList :: Board -> Position -> Player -> Direction -> [Position] -> [Position]
  getFlipList board pos player dir acc
    | nextPiece == Empty || nextPos == pos = []
    | nextPiece == opPiece =
      getFlipList board nextPos player dir (nextPos:acc)
    | otherwise = acc
    where
      nextPos = moveDir pos dir
      nextPiece = snd (board !! (idx (moveDir pos dir)))
      opPiece
        | player == PlayerW = Black
        | otherwise = White

  getAllFlipLists :: Board -> Position -> Player -> [Position]
  getAllFlipLists board (x, y) player =
    (getFlipList board (x, y) player N [])++
    (getFlipList board (x, y) player NE [])++
    (getFlipList board (x, y) player E [])++
    (getFlipList board (x, y) player SE [])++
    (getFlipList board (x, y) player S [])++
    (getFlipList board (x, y) player SW [])++
    (getFlipList board (x, y) player W [])++
    (getFlipList board (x, y) player NW [])

  move :: Board -> Position -> Player -> Board
  move board pos player
    | checkValid board pos player =
      replace board posList player
    | otherwise = board
    where
      posList = pos:(getAllFlipLists board pos player)

  replace :: Board -> [Position] -> Player -> Board
  replace [] _ _ = []
  replace ((curPos, curPiece):rest) posList player
    | curPos `elem` posList = (curPos, piece):(replace rest posList player)
    | otherwise = (curPos, curPiece):(replace rest posList player)
    where
      piece
        | player == PlayerW = White
        | otherwise = Black