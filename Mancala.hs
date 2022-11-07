import Debug.Trace
import Data.Maybe

type Bean = Int
type Slot = Bean 
data Player = P1 | P2 deriving (Show, Eq)
data Board = Board {goalP1 :: Slot, slotsP1 :: [Slot], 
                    goalP2 :: Slot, slotsP2 :: [Slot], playerTurn :: Player} deriving (Show, Eq) --add record notation 
data Outcome = Turn | Winner Player | Tie 

board = Board {goalP1 = 0, slotsP1 = [4,4,4,4,4,4], goalP2 = 0, slotsP2 = [4,4,4,4,4,4], playerTurn = P2}

--Show Function
------------------------------------
-- ha ha funny show function
showBoard :: Board -> String
showBoard Board {goalP1 = g1, slotsP1 = s1, goalP2 = g2, slotsP2 = s2, playerTurn = p} =if p == P1 then  "    (6) (5) (4) (3) (2) (1) \n" ++ body else body ++ "    (1) (2) (3) (4) (5) (6)\n"
  where border = "@><><><><><><><@><><><><><><><@\n"
        body = border ++ "|  "++displaySideOne s1++"|  |\n"++ "|"++show g1++    " |-----------------------| "++show g2++"|\n" ++ "|  "++displaySideTwo s2 ++"|  |\n"++ border


displaySideOne :: [Slot] -> String
displaySideOne [] = ""
displaySideOne (b:bs) = displaySideOne bs ++ "| "++show b++" "

displaySideTwo :: [Slot] -> String
displaySideTwo [] = ""
displaySideTwo (b:bs) = "| "++show b++" "++displaySideTwo bs


--Gameplay Functions 
----------------------------------

across :: Int -> Int
across = undefined

-- pattern match and return the opposite slot for every position

move :: Board -> Int -> Board
move = undefined

-- takes a board and a position and return an updated board

checkCapture :: Board -> Board
checkCapture = undefined

-- checks if it is possible to capture a piece and if so changs the board


-- GameState Functions
------------------------------------


updateOutcome :: Board -> Outcome
updateOutcome = undefined

-- takes a board and returns: Turn, Winner, or Tie
















