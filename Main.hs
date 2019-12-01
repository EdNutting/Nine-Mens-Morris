import Utils
import Board
import Player
import Move

import Phase1
import Phase2
import Phase3

checkBoard :: Board -> IO Board
checkBoard b =
    if checkBoardSetup b
    then return b
    else fail "Board setup not valid!"

initGame :: IO Board
initGame =
    do b <- checkBoard standardBoard
       if playerMaxPins <= 0
       then fail "playerMaxPins not greater than 0."
       else do printBoard b
               return b

endGame :: (Player, Board) -> IO ()
endGame (p, b) = putStrLn (show p ++ " wins!")

twoPhase :: IO ()
twoPhase = initGame
       >>= phase1
       >>= phase2 2
       >>= endGame

threePhase :: IO ()
threePhase = initGame
        >>= phase1
        >>= phase2 3
        >>= phase3
        >>= endGame

main :: IO ()
main = askQuestion "Would you like to play a 2 or 3 phase game?"
                   [Option "2" "Two phase game" twoPhase,
                    Option "3" "Three phase game" threePhase
                   ]
                   True
