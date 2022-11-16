import Debug.Trace
import Data.Maybe

type Bean = Int
type Slot = Bean
data Player = P1 | P2 deriving (Show, Eq)

data Board = Board {goalP1 :: Slot, slotsP1 :: [Slot], 
                    goalP2 :: Slot, slotsP2 :: [Slot], playerTurn :: Player} deriving (Show, Eq) --add record notation 
data Outcome = Turn | Winner Player | Tie 

board = Board {goalP1 = 0, slotsP1 = [4,4,1,4,4,4], goalP2 = 0, slotsP2 = [4,4,4,4,4,4], playerTurn = P1}

--Show Function
------------------------------------
-- ha ha funny show function: use putStr (showBoard board) when trying to print in ghci
showBoard :: Board -> String
showBoard Board {goalP1 = g1, slotsP1 = s1, goalP2 = g2, slotsP2 = s2, playerTurn = p} =if p == P1 then  "    (6) (5) (4) (3) (2) (1) \n" ++ body else body ++ "    (1) (2) (3) (4) (5) (6)\n"
  where border = "@><><><><><><><@><><><><><><><@\n"
        body = border ++ "|  "++displaySideOne s1++"|  |\n"++ "|"++show g1++    " |-----------------------| "++show g2++"|\n" ++ "|  "++displaySideTwo s2 ++"|  |\n"++ border

-- displayes slotsP1 backwards with border 
displaySideOne :: [Slot] -> String
displaySideOne [] = ""
displaySideOne (b:bs) = displaySideOne bs ++ "| "++show b++" "

-- displays slotsP2 with border
displaySideTwo :: [Slot] -> String
displaySideTwo [] = ""
displaySideTwo (b:bs) = "| "++show b++" "++displaySideTwo bs


--Gameplay Functions 
----------------------------------

across :: Int -> Int
across x = if x == 7 then 14 else 14 - x

move :: Board -> Int -> Maybe Board
move brd pos = if pos `elem` filtered then Just (makeMove brd pos) else Nothing
  where moves = getSide brd
        filtered = [x | x <- [1..6], not ((fst moves) !! (x-1) == 0)]

-- pattern match and return the opposite slot for every position
makeMove :: Board -> Int -> Board
makeMove brd pos =
  let (Board g1 s1 g2 s2 p) = brd
      (slots,x) = getSide (Board g1 s1 g2 s2 p)
      (s, beans) = insideMovement slots pos
  in if x == 2 then  executePlay (Board g1 s g2 s2 p) beans 1 else executePlay (Board g1 s1 g2 s p) beans 3


-- gets the player's [Slot]
getSide :: Board -> ([Slot],Int)
getSide (Board g1 s1 g2 s2 p) = if p == P1 then (s1,2) else (s2,4)


-- gets the other player's [Slot] 
getOtherSide :: Board -> [Slot]
getOtherSide (Board g1 s1 g2 s2 p) = if p == P1 then s2 else s1

--Moves the Peices on one side of the board, does not delete the starting peice
--needs to be renamed 
sideMovement :: [Slot] -> Int -> Bean -> [Slot]
--sideMovement _ _ 0 = error "Shouldnt Have a zero bean input"
sideMovement slots pos beans = splitAndRebuild slots beans


--adds 1 to every index until it runs out of beans or reaches the end of the list
splitAndRebuild :: [Slot] -> Bean -> [Slot]
splitAndRebuild slots 0 = slots
splitAndRebuild [] x = []
splitAndRebuild (b:bs) beans = (b+1):(splitAndRebuild bs (beans-1))



--Does operations for the first part of a turn
--needs to be renamed
insideMovement :: [Slot] -> Int -> ([Slot],Bean)
insideMovement slots pos = (newFront front ++ sideMovement back (pos-1) beans, beans-(6-pos))
  where splice = splitAt pos slots
        front = fst splice
        back = snd splice
        beans = last front


-- updates the front of a [Slot] to show that a slot has been emptied
newFront :: [Slot] -> [Slot]
newFront [b] = [0]
newFront (b:bs) = b:(newFront bs)


-- continues distributing beans until it runs out
executePlay :: Board -> Bean -> Int -> Board
executePlay (Board g1 s1 g2 s2 p) beans 1 = if beans > 0 then if p == P1 then executePlay (Board (g1+1) s1 g2 s2 p) (beans-1) 4 else executePlay (Board g1 s1 g2 s2 p) (beans) 4 else Board g1 s1 g2 s2 p
executePlay (Board g1 s1 g2 s2 p) beans 2 = if beans > 0 then executePlay (Board g1 (sideMovement s1 1 beans) g2 s2 p) (beans-6) 1 else Board g1 s1 g2 s2 p
executePlay (Board g1 s1 g2 s2 p) beans 3 = if beans > 0 then if p == P2 then executePlay (Board g1 s1 (g2+1) s2 p) (beans-1) 2 else executePlay (Board g1 s1 g2 s2 p) (beans) 2 else Board g1 s1 g2 s2 p
executePlay (Board g1 s1 g2 s2 p) beans 4 = if beans > 0 then executePlay (Board g1 s1 g2 (sideMovement s2 1 beans) p) (beans-6) 3 else Board g1 s1 g2 s2 p

-- check if a capture is possible and if so then caputre else return the board
checkCapture :: Board -> Int -> Board
checkCapture brd pos =
  let currentSide = getSide brd
  in if (fst currentSide !! (pos - 1)) == 1 then capture brd pos else brd

-- does the operations for checkCapture
-- needs to take a FauxBoard
capture :: Board -> Int -> Board
capture brd pos =
  let currentSide = getSide brd
      otherSide = getOtherSide brd
      goal1 (Board g1 _ _ _ p) beans = if p == P1 then g1 + beans else g1
      goal2 (Board _ _ g2 _ p) beans = if p == P2 then g2 + beans else g2
      beanTotal = 1 + (otherSide !! (pos-1))
  in  Board (goal1 brd beanTotal) (makePosZero (getSide1 brd) pos) (goal2 brd beanTotal) (makePosZero (getSide2 brd) pos) (getPlayer (brd))

-- returns player
getPlayer :: Board -> Player
getPlayer (Board _ _ _ _ p) = p

--returns slotsP1
getSide1 :: Board -> [Slot]
getSide1 (Board _ s1 _ _ _) = s1

--returns slotsP2
getSide2 :: Board -> [Slot]
getSide2 (Board _ _ _ s2 _) = s2

-- changes the value of a slot at a position to 0
makePosZero :: [Slot] -> Int -> [Slot]
makePosZero slots pos = frontHalf ++ [0] ++ backHalf
  where frontHalf = init (fst(splitAt (pos) slots))
        backHalf = snd(splitAt (pos) slots)


-- checks if it is possible to capture a piece and if so changs the board


-- GameState Functions
------------------------------------

--takes a board and returns: Turn, Winner, or Tie
updateOutcome :: Board -> Outcome
updateOutcome Board {slotsP1 = s1, slotsP2 = s2, playerTurn = p} = if not (sum s1 == 0 && sum s2 == 0) then Turn else getWinner board 


-- helper function
getWinner :: Board -> Outcome
getWinner Board {goalP1 = g1, goalP2 = g2} 
  | g1 > g2 = Winner P1
  | g2 > g1 = Winner P2
  | otherwise = Tie

-- takes a board and returns a board with the other player
updateTurn :: Board -> Board
updateTurn Board {slotsP1 = s1, goalP1 = g1, slotsP2 = s2, goalP2 = g2, playerTurn = p} = if p == P1 then Board g1 s1 g2 s2 P2 else Board g1 s1 g2 s2 P1


