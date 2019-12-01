module Move where
    import Utils
    import Board
    import Player

    data Move = Move (Maybe Location) Location
              deriving (Eq, Show, Read)

    locationToOption :: (Location -> IO a) -> Location -> Option a
    locationToOption action l@(Location r c) =
        Option (show c ++ ", " ++ show r)
               "Available location"
               (action l)

    moveToOption :: (Move -> IO a) -> Move -> Option a
    moveToOption action m@(Move Nothing (Location r c)) =
        Option (show c ++ ", " ++ show r)
               "Available location"
               (action m)
    moveToOption action m@(Move (Just (Location r c)) (Location r' c')) =
        Option (show c ++ ", " ++ show r ++ " -> " ++ show c' ++ ", " ++ show r')
               "Available move"
               (action m)

    locationToMove :: Location -> Move
    locationToMove l = Move Nothing l

    locationsToMoves :: [Location] -> [Move]
    locationsToMoves = map locationToMove

    locationsToOptions :: (Location -> IO a) -> [Location] -> [Option a]
    locationsToOptions action = map (locationToOption action)

    movesToOptions :: (Move -> IO a) -> [Move] -> [Option a]
    movesToOptions action = map (moveToOption action)

    isHole :: Location -> Board -> Bool
    isHole (Location r c) b
        | r >= 0 && r < length b && c >= 0 && c < length b =
        case ((b !! r) !! c) of
            (Just _) -> True
            _ -> False
    isHole _ _ = False

    isEmptyHole :: Location -> Board -> Bool
    isEmptyHole (Location r c) b
        | r >= 0 && r < length b && c >= 0 && c < length b =
        case ((b !! r) !! c) of
            (Just Empty) -> True
            _ -> False
    isEmptyHole _ _ = False

    isPlayerAtLocation :: Location -> Player -> Board -> Bool
    isPlayerAtLocation (Location r c) p b
        | r >= 0 && r < length b && c >= 0 && c < length b =
        case ((b !! r) !! c) of
            (Just (Pin p')) -> p == p'
            _ -> False
    isPlayerAtLocation _ _ _ = False

    searchForPlayer :: Location -> Int -> Bool -> Player -> Board -> Maybe Location
    -- Vertical
    searchForPlayer l@(Location r c) offset True p b
        | r >= 0 && r < length b && c >= 0 && c < length b =
        if isHole l b
        then if isPlayerAtLocation l p b
             then Just l
             else Nothing
        else if (r /= halfway)
                || (signum (r - halfway) == signum (r + offset - halfway))
             then searchForPlayer (Location (r + offset) c) offset True p b
             else Nothing
        where halfway = length b `div` 2

    -- Horizontal
    searchForPlayer l@(Location r c) offset False p b
        | r >= 0 && r < length b && c >= 0 && c < length b =
        if isHole l b
        then if isPlayerAtLocation l p b
             then Just l
             else Nothing
        else if (c /= halfway)
                || (signum (c - halfway) == signum (c + offset - halfway))
             then searchForPlayer (Location r (c + offset)) offset False p b
             else Nothing
        where halfway = length b `div` 2

    searchForPlayer _ _ _ _ _ = Nothing


    searchForEmptyHole :: Location -> Int -> Bool -> Board -> Maybe Location
    -- Vertical
    searchForEmptyHole l@(Location r c) offset True b
        | r >= 0 && r < length b && c >= 0 && c < length b =
        if isHole l b
        then if isEmptyHole l b
             then Just l
             else Nothing
        else if (r /= halfway)
                || (signum (r - halfway) == signum (r + offset - halfway))
             then searchForEmptyHole (Location (r + offset) c) offset True b
             else Nothing
        where halfway = length b `div` 2

    -- Horizontal
    searchForEmptyHole l@(Location r c) offset False b
        | r >= 0 && r < length b && c >= 0 && c < length b =
        if isHole l b
        then if isEmptyHole l b
             then Just l
             else Nothing
        else if (c /= halfway)
                || (signum (c - halfway) == signum (c + offset - halfway))
             then searchForEmptyHole (Location r (c + offset)) offset False b
             else Nothing
        where halfway = length b `div` 2

    -- Out of range cases
    searchForEmptyHole _ _ _ _ = Nothing

    countMillV :: Location -> Player -> Board -> Int
    countMillV l p b =
        m1 + m2 + m3
        where
            toBool = maybe False (const True)
            boolToInt b = if b then 1 else 0

            m1 = boolToInt (isPlayerAtLocation l p b && toBool mLP2)
            m2 = boolToInt (isPlayerAtLocation l p b && toBool mLM2)
            m3 = boolToInt (isPlayerAtLocation l p b && toBool mLP1 && toBool mLM1)

            incrementLoc :: Location -> Location
            incrementLoc (Location r c) = Location (r + 1) c

            decrementLoc :: Location -> Location
            decrementLoc (Location r c) = Location (r - 1) c

            mLP1 = searchForPlayer (incrementLoc l) 1 True p b
            mLP2 = maybe Nothing (\ l' -> searchForPlayer (incrementLoc l') 1 True p b) mLP1

            mLM1 = searchForPlayer (decrementLoc l) (-1) True p b
            mLM2 = maybe Nothing (\ l' -> searchForPlayer (decrementLoc l') (-1) True p b) mLM1

    countMillH :: Location -> Player -> Board -> Int
    countMillH l p b = 
        m1 + m2 + m3
        where
            toBool = maybe False (const True)
            boolToInt b = if b then 1 else 0

            m1 = boolToInt (isPlayerAtLocation l p b && toBool mLP2)
            m2 = boolToInt (isPlayerAtLocation l p b && toBool mLM2)
            m3 = boolToInt (isPlayerAtLocation l p b && toBool mLP1 && toBool mLM1)

            incrementLoc :: Location -> Location
            incrementLoc (Location r c) = Location r (c + 1)

            decrementLoc :: Location -> Location
            decrementLoc (Location r c) = Location r (c - 1)

            mLP1 = searchForPlayer (incrementLoc l) 1 False p b
            mLP2 = maybe Nothing (\ l' -> searchForPlayer (incrementLoc l') 1 False p b) mLP1

            mLM1 = searchForPlayer (decrementLoc l) (-1) False p b
            mLM2 = maybe Nothing (\ l' -> searchForPlayer (decrementLoc l') (-1) False p b) mLM1

    countMills :: Location -> Player -> Board -> Int
    countMills l p b = countMillV l p b + countMillH l p b

    listPlayerPinsNotInMill :: Player -> Board -> [Location]
    listPlayerPinsNotInMill p b = filter (\ l -> countMills l p b == 0) (listPlayerPins p b)


    emptyLocationsFrom :: Location -> Board -> [Location]
    emptyLocationsFrom (Location r c) b =
        mToList mUp ++ mToList mDown ++ mToList mLeft ++ mToList mRight
        where
            mToList = maybe [] (\ x -> x : [])

            lUp = Location (r - 1) c
            lDown = Location (r + 1) c
            lLeft = Location r (c - 1)
            lRight = Location r (c + 1)

            mUp = searchForEmptyHole lUp (-1) True b
            mDown = searchForEmptyHole lDown 1 True b
            
            mLeft = searchForEmptyHole lLeft (-1) False b
            mRight = searchForEmptyHole lRight 1 False b

    listAvailableMoves :: Player -> Board -> [Move]
    listAvailableMoves p b =
        availableMoves' (length b - 1) (length b - 1)
        where
            availableMoves' :: Int -> Int -> [Move]
            availableMoves' (-1) (-1) = []
            availableMoves' r (-1) = availableMoves' (r - 1) (length b - 1)
            availableMoves' r c =
                (if isPlayerAtLocation (Location r c) p b
                 then map (Move (Just (Location r c))) (emptyLocationsFrom (Location r c) b)
                 else [])
                ++ availableMoves' r (c - 1)


    setHole :: Location -> Hole -> Board -> IO Board
    setHole (Location r c) h rows =
        return (updateAt r setCol rows)
        where
            setCol = updateAt c (const (Just h))

    placePin :: Location -> Player -> Board -> IO (Board, Int)
    placePin dst p b0 =
        do b1 <- setHole dst (Pin p) b0
           return (b1 , countMills dst p b1)

    removePin :: Location -> Board -> IO Board
    removePin src = setHole src Empty

    movePin :: Location -> Location -> Player -> Board -> IO (Board, Int)
    movePin src dst p b0 =
        do b1 <- removePin src b0
           placePin dst p b1

    doPlaceOrMove :: Player -> Board -> [Move] -> IO (Board, Int)
    doPlaceOrMove p b [] =
        do putStrLn "No available moves"
           return (b , 0)
    doPlaceOrMove p b moves =
        do askQuestion ("Where would you like to " ++ word ++ " a pin? " ++ format)
                       moveOptions
                       False
        where
            word = case head moves of
                        (Move Nothing _) -> "place"
                        _ -> "move"

            format = case head moves of
                          (Move Nothing _) -> "'x, y'"
                          _ -> "'x, y -> x, y'"

            doPlaceOrMove' :: Move -> IO (Board, Int)
            doPlaceOrMove' (Move Nothing dst) =
                placePin dst p b
            doPlaceOrMove' (Move (Just src) dst) =
                movePin src dst p b

            moveOptions = movesToOptions doPlaceOrMove' moves

    doRemove :: Board -> [Location] -> IO Board
    doRemove b [] =
        do putStrLn "No available moves"
           return b
    doRemove b locations =
        do askQuestion "Which piece would you like to remove? 'x, y'"
                       locationOptions
                       False
        where
            doRemove' :: Location -> IO Board
            doRemove' l = removePin l b

            locationOptions = locationsToOptions doRemove' locations

    countPieces :: Board -> IO (Int, Int)
    countPieces b =
        return ( countPlayerPins Player1 b
               , countPlayerPins Player2 b)

    printCountPlacedPieces :: Board -> IO ()
    printCountPlacedPieces b =
        do (p1C, p2C) <- countPieces b
           putStrLn ("Player 1: " ++ show p1C ++ " pieces on the board.")
           putStrLn ("Player 2: " ++ show p2C ++ " pieces on the board.")

    removeOppositionPieces :: Int -> Player -> Board -> IO Board
    removeOppositionPieces 0 _ b = return b
    removeOppositionPieces count currP b = 
        do putStrLn "You formed a mill."
           let locs = listPlayerPinsNotInMill (nextPlayer currP) b
           if length locs == 0
           then do putStrLn "Unfortunately, there are no pins that may be removed."
                   return b
           else do printBoard b
                   b1 <- doRemove b (listPlayerPinsNotInMill (nextPlayer currP) b)
                   removeOppositionPieces (count - 1) currP b1