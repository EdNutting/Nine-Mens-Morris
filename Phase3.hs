module Phase3 where
    import Utils
    import Board
    import Player
    import Move

    phase3_loop :: Player -> Board -> IO (Player, Board)
    phase3_loop p b =
        do if countPlayerPins p b < 3
           then return (nextPlayer p , b)
           else do let emptyHoles = listEmptyHoles b
                   let playerPins = listPlayerPins p b
                   let moves = concatMap (\ l -> map (Move (Just l)) emptyHoles) playerPins
                   if length moves == 0
                   then return (nextPlayer p , b)
                   else do putStrLn (show p ++ ":")
                           (b1, millsCount) <- doPlaceOrMove p b moves
                           b2 <- removeOppositionPieces millsCount p b1
                           printBoard b2
                           printCountPlacedPieces b2
                           phase3_loop (nextPlayer p) b2

    phase3 :: (Player, Board) -> IO (Player, Board)
    phase3 (p, b) =
        do putStrLn "Phase 3 : Moving pins with flying"
           phase3_loop p b
