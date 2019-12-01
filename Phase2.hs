module Phase2 where
    import Utils
    import Board
    import Player
    import Move

    phase2_loop :: Int -> Player -> Board -> IO (Player, Board)
    phase2_loop minPieces p b =
        do if countPlayerPins p b <= minPieces
           then return (nextPlayer p , b)
           else do let moves = listAvailableMoves p b
                   if length moves == 0
                   then return (nextPlayer p , b)
                   else do putStrLn (show p ++ ":")
                           (b1, millsCount) <- doPlaceOrMove p b moves
                           b2 <- removeOppositionPieces millsCount p b1
                           printBoard b2
                           printCountPlacedPieces b2
                           phase2_loop minPieces (nextPlayer p) b2

    phase2 :: Int -> Board -> IO (Player, Board)
    phase2 minPieces b =
        do putStrLn "Phase 2 : Moving pins without flying"
           phase2_loop minPieces Player1 b
