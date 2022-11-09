import Debug.Trace
import Data.Maybe

type Bean = Int
type Slot = Bean 
data Player = P1 | P2 deriving (Show, Eq)
data Board = Board {goalP1 :: Slot, slotsP1 :: [Slot], 
                    goalP2 :: Slot, slotsP2 :: [Slot], playerTurn :: Player} deriving (Show, Eq) --add record notation 
data Outcome = Turn | Winner Player | Tie 

board = Board {goalP1 = 0, slotsP1 = [4,4,4,4,4,4], goalP2 = 0, slotsP2 = [4,4,4,4,4,4], playerTurn = P1}

--Show Function
------------------------------------
-- ha ha funny show function: use putStr (showBoard board) when trying to print in ghci
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


-- step 1: take all beans out of slot
-- step 2: increment over list leaving one bean at a time
-- step 3: if you get to a goal, check whose it is and then leave a bean or continue
-- step 4: increment over list leaving one bean at a time
-- step 5: return updated board
-- takes a board and a position and return an updated board

checkCapture :: Board -> Board
checkCapture = undefined

-- checks if it is possible to capture a piece and if so changs the board


-- GameState Functions
------------------------------------


updateOutcome :: Board -> Outcome
updateOutcome Board {sideP1 = s1, sideP2 = s2, playerTurn = p} = if not (sum s1 && sum s2 == 0) then Turn else getWinner board 


-- helper function
getWinner :: Board -> Outcome
getWinner Board {goalP1 = g1, goalP2 = g2} 
  | g1 > g2 = Winner P1
  | g2 > g1 = Winner P2
  | otherwise = Tie

-- takes a board and returns: Turn, Winner, or Tie
updateTurn :: Board -> Board
updateTurn Board {sideP1 = s1, goalP1 = g1, sideP2 = s2, goalP2 = g2, playerTurn = p} = if p == P1 then Board s1 g1 s2 g2 P2 else Board s1 g1 s2 g2 P1















