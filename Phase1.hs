module Phase1 where
    import Utils
    import Board
    import Player
    import Move

    phase1Move :: Player -> Board -> IO (Board, Int)
    phase1Move p b =
        do putStrLn (show p ++ ":")
           doPlaceOrMove p b (locationsToMoves $ listEmptyHoles b)

    phase1_step :: Player -> Board -> IO Board
    phase1_step p b0 =
        do (b1 , millsCount) <- phase1Move p b0
           b2 <- removeOppositionPieces millsCount p b1
           printBoard b2
           printCountPlacedPieces b2
           return b2

    phase1_loop :: Int -> Board -> IO Board
    phase1_loop c b0 =
        do b1 <- phase1_step Player1 b0
           b2 <- phase1_step Player2 b1
           putStrLn (show (c - 1) ++ " pieces remaining to place.")
           if (c - 1) == 0
           then return b2
           else phase1_loop (c - 1) b2

    phase1 :: Board -> IO Board
    phase1 b =
        do putStrLn "Phase 1 : Placing pins"
           phase1_loop playerMaxPins b
