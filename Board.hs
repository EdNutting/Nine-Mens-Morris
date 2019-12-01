module Board where
    import Data.Foldable (foldlM)

    import Player

    -- Expected to be > 0
    playerMaxPins :: Int
    playerMaxPins = 9

    data Location = Location Int Int
                  deriving (Eq, Show, Read)

    -- Holes may be empty or contain the pin of the first or second player
    data Hole = Empty
              | Pin Player
              deriving (Eq, Show, Read)

    -- Boards are represented as an adjacency matrix of holes
    -- A hole needn't exist in every location - the board is not a total grid
    -- Edges bridge non-existent holes in N/S/E/W directions but not diagonals
    type Column = Maybe Hole
    type Row = [Column]
    type Board = [Row]

    emptyHole :: Maybe Hole
    emptyHole = Just Empty

    noHole :: Maybe Hole
    noHole = Nothing

    p1Pin :: Maybe Hole
    p1Pin = Just (Pin Player1)

    p2Pin :: Maybe Hole
    p2Pin = Just (Pin Player2)

    standardBoard :: Board
    standardBoard
        = [ [ emptyHole , noHole    , noHole    , emptyHole , noHole    , noHole    , emptyHole ]
          , [ noHole    , emptyHole , noHole    , emptyHole , noHole    , emptyHole , noHole    ]
          , [ noHole    , noHole    , emptyHole , emptyHole , emptyHole , noHole    , noHole    ]
          , [ emptyHole , emptyHole , emptyHole , noHole    , emptyHole , emptyHole , emptyHole ]
          , [ noHole    , noHole    , emptyHole , emptyHole , emptyHole , noHole    , noHole    ]
          , [ noHole    , emptyHole , noHole    , emptyHole , noHole    , emptyHole , noHole    ]
          , [ emptyHole , noHole    , noHole    , emptyHole , noHole    , noHole    , emptyHole ]
          ]

    testMillBoard :: Board
    testMillBoard
        = [ [ p1Pin     , noHole    , noHole    , p1Pin     , noHole    , noHole    , p1Pin     ]
          , [ noHole    , emptyHole , noHole    , emptyHole , noHole    , emptyHole , noHole    ]
          , [ noHole    , noHole    , emptyHole , emptyHole , emptyHole , noHole    , noHole    ]
          , [ emptyHole , emptyHole , emptyHole , noHole    , emptyHole , emptyHole , emptyHole ]
          , [ noHole    , noHole    , emptyHole , emptyHole , emptyHole , noHole    , noHole    ]
          , [ noHole    , emptyHole , noHole    , emptyHole , noHole    , emptyHole , noHole    ]
          , [ emptyHole , noHole    , noHole    , emptyHole , noHole    , noHole    , emptyHole ]
          ]

    testMillBoard2 :: Board
    testMillBoard2
        = [ [ emptyHole , noHole    , noHole    , p1Pin     , noHole    , noHole    , emptyHole ]
          , [ noHole    , emptyHole , noHole    , p1Pin     , noHole    , emptyHole , noHole    ]
          , [ noHole    , noHole    , emptyHole , p1Pin     , emptyHole , noHole    , noHole    ]
          , [ emptyHole , emptyHole , emptyHole , noHole    , emptyHole , emptyHole , emptyHole ]
          , [ noHole    , noHole    , emptyHole , emptyHole , emptyHole , noHole    , noHole    ]
          , [ noHole    , emptyHole , noHole    , emptyHole , noHole    , emptyHole , noHole    ]
          , [ emptyHole , noHole    , noHole    , emptyHole , noHole    , noHole    , emptyHole ]
          ]

    testMillBoard3 :: Board
    testMillBoard3
        = [ [ emptyHole , noHole    , noHole    , emptyHole , noHole    , noHole    , emptyHole ]
          , [ noHole    , emptyHole , noHole    , emptyHole , noHole    , emptyHole , noHole    ]
          , [ noHole    , noHole    , emptyHole , emptyHole , emptyHole , noHole    , noHole    ]
          , [ emptyHole , emptyHole , emptyHole , noHole    , emptyHole , emptyHole , emptyHole ]
          , [ noHole    , noHole    , emptyHole , p1Pin     , emptyHole , noHole    , noHole    ]
          , [ noHole    , emptyHole , noHole    , p1Pin     , noHole    , emptyHole , noHole    ]
          , [ emptyHole , noHole    , noHole    , p1Pin     , noHole    , noHole    , emptyHole ]
          ]

    testMillBoard4 :: Board
    testMillBoard4
        = [ [ p1Pin     , noHole    , noHole    , p1Pin     , noHole    , noHole    , p1Pin     ]
          , [ noHole    , emptyHole , noHole    , p1Pin     , noHole    , p2Pin     , noHole    ]
          , [ noHole    , noHole    , p2Pin     , p1Pin     , p2Pin     , noHole    , noHole    ]
          , [ p2Pin     , p2Pin     , p2Pin     , noHole    , p2Pin     , emptyHole , emptyHole ]
          , [ noHole    , noHole    , p2Pin     , p1Pin     , p2Pin     , noHole    , noHole    ]
          , [ noHole    , emptyHole , noHole    , p1Pin     , noHole    , emptyHole , noHole    ]
          , [ p1Pin     , noHole    , noHole    , p1Pin     , noHole    , noHole    , emptyHole ]
          ]
          
    testMillBoard5 :: Board
    testMillBoard5
        = [ [ p1Pin     , noHole    , noHole    , p1Pin     , noHole    , noHole    , p1Pin     ]
          , [ noHole    , p1Pin     , noHole    , p1Pin     , noHole    , p1Pin     , noHole    ]
          , [ noHole    , noHole    , p1Pin     , p1Pin     , p1Pin     , noHole    , noHole    ]
          , [ p2Pin     , p2Pin     , p2Pin     , noHole    , p2Pin     , p2Pin     , p2Pin     ]
          , [ noHole    , noHole    , emptyHole , emptyHole , emptyHole , noHole    , noHole    ]
          , [ noHole    , p2Pin     , noHole    , p2Pin     , noHole    , p2Pin     , noHole    ]
          , [ emptyHole , noHole    , noHole    , emptyHole , noHole    , noHole    , emptyHole ]
          ]

    testNoMillBoard :: Board
    testNoMillBoard
        = [ [ emptyHole , noHole    , noHole    , emptyHole , noHole    , noHole    , emptyHole ]
          , [ noHole    , emptyHole , noHole    , emptyHole , noHole    , emptyHole , noHole    ]
          , [ noHole    , noHole    , emptyHole , p1Pin     , emptyHole , noHole    , noHole    ]
          , [ emptyHole , emptyHole , emptyHole , noHole    , emptyHole , emptyHole , emptyHole ]
          , [ noHole    , noHole    , emptyHole , p1Pin     , emptyHole , noHole    , noHole    ]
          , [ noHole    , emptyHole , noHole    , p1Pin     , noHole    , emptyHole , noHole    ]
          , [ emptyHole , noHole    , noHole    , emptyHole , noHole    , noHole    , emptyHole ]
          ]

    checkBoardSetup :: Board -> Bool
    checkBoardSetup []
        = False
    checkBoardSetup (_ : [])
        = False
    checkBoardSetup (r1 : r2 : [])
        = length r1 == length r2
    checkBoardSetup (r1 : r2 : rs)
        =  length r1 == length r2
        && checkBoardSetup (r2 : rs)

    printBoard :: Board -> IO ()
    printBoard b =
        do putStrLn ("  | " ++ toColumnsStr 0 (length (head b)))
           foldlM (\ c r -> do printRow r c
                               return (1 + c))
                  0
                  b
           return ()
        where
            printRow :: Row -> Int -> IO ()
            printRow r rIdx =
                do putStr (show rIdx ++ " |")
                   foldlM (\ c col -> do printCol col rIdx c
                                         return (1 + c))
                          0
                          r
                   putStrLn ""
                   return ()

            printCol :: Column -> Int -> Int -> IO ()
            printCol Nothing rIdx cIdx = putStr "    "
            printCol (Just Empty) rIdx cIdx = putStr " .  "
            printCol (Just (Pin Player1)) rIdx cIdx = putStr " 1  "
            printCol (Just (Pin Player2)) rIdx cIdx = putStr " 2  "

            toColumnsStr :: Int -> Int -> String
            toColumnsStr _ 0 = ""
            toColumnsStr idx cnt =
                let rest = toColumnsStr (idx + 1) (cnt - 1) in
                if rest == ""
                then show idx
                else show idx ++ " | " ++ rest

    listEmptyHoles :: Board -> [Location]
    listEmptyHoles b = rowLocations b 0
        where
            rowLocations :: Board -> Int -> [Location]
            rowLocations [] rIdx
                = []
            rowLocations (r : rs) rIdx
                = columnLocations r rIdx 0 ++ rowLocations rs (1 + rIdx)

            columnLocations :: [Column] -> Int -> Int -> [Location]
            columnLocations [] rIdx cIdx
                = []
            columnLocations (Just Empty : cs) rIdx cIdx
                = Location rIdx cIdx : columnLocations cs rIdx (1 + cIdx)
            columnLocations (_ : cs) rIdx cIdx
                = columnLocations cs rIdx (1 + cIdx)

    listPlayerPins :: Player -> Board -> [Location]
    listPlayerPins p b = rowLocations b 0
        where
            rowLocations :: Board -> Int -> [Location]
            rowLocations [] rIdx
                = []
            rowLocations (r : rs) rIdx
                = columnLocations r rIdx 0 ++ rowLocations rs (1 + rIdx)

            columnLocations :: [Column] -> Int -> Int -> [Location]
            columnLocations [] rIdx cIdx
                = []
            columnLocations (Just (Pin p') : cs) rIdx cIdx
                | p == p'
                = Location rIdx cIdx : columnLocations cs rIdx (1 + cIdx)
            columnLocations (_ : cs) rIdx cIdx
                = columnLocations cs rIdx (1 + cIdx)

    countPlayerPins :: Player -> Board -> Int
    countPlayerPins p b = foldr (\ r c -> c + countPlayerPinsR r) 0 b
        where
            countPlayerPinsR :: Row -> Int
            countPlayerPinsR = foldr (\ col c -> c + countPlayerPinsC col) 0

            countPlayerPinsC :: Column -> Int
            countPlayerPinsC (Just (Pin p')) =
                if p == p'
                then 1
                else 0
            countPlayerPinsC _ = 0
