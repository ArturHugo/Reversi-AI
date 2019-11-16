module AI where

  import Board (Position, Piece(White, Black, Empty), Board, width, height, positions)
  import Play

  moveScore :: Board -> Position -> Player -> Int
  moveScore board pos player = length $ getAllFlipLists board pos player

  allMoveScores :: Board -> [Position] -> Player -> [(Position, Int)]
  allMoveScores _ [] _ = []
  allMoveScores board (curPos:restPos) player =
    (curPos, moveScore board curPos player):(allMoveScores board restPos player)

  validMoveScores :: Board -> [Position] -> Player -> [(Position, Int)]
  validMoveScores board positions player =
    [scores !! i | i <- [0..(length scores)-1], (snd (scores !! i)) /= 0]
    where
      scores = allMoveScores board positions player

  getBestPos :: [(Position, Int)] -> Position
  getBestPos scores =
    fst $ scores !! (snd . maximum $ zip (snd $ unzip scores) [0 .. ])

  minimax :: Board -> [Position] -> Player -> Bool -> Int -> Int -> (Position, Int)
  minimax board positions player isMax score 1
    | isMax =
      (targetPos, (moveScore board targetPos player))
    | otherwise = (targetPos, -1*(moveScore board targetPos player))
    where
      targetPos = getBestPos (validMoveScores board positions player)

  minimax board positions player isMax score depth
    | isMax =
      (targetPos, moveScore board targetPos player + (snd (minimax newBoard positions otherPlayer False totalScore (depth-1))))
    | otherwise = (targetPos, -1*(moveScore board targetPos player) + (snd (minimax newBoard positions otherPlayer True totalScore (depth-1))))
    where
      otherPlayer
        | player == PlayerW = PlayerB
        | otherwise = PlayerW
      totalScore = score + (snd (getBestPos (validMoveScores board positions player)))
      targetPos = getBestPos (validMoveScores board positions player)
      newBoard = move board targetPos player
