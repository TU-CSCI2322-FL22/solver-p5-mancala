import Debug.Trace
import Data.Maybe

type Bean = Int
type Slot = Bean
type FauxBoard = (Slot, [Slot], Slot, [Slot], Player)
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
across x = if x == 7 then 14 else 14 - x

----Board {goalP1 = g1, slotsP1 = s1, goalP2 = g2, slotsP2 = s2, playerTurn = p}
---- pattern match and return the opposite slot for every position
--move :: Board -> Int -> Board
--move brd pos = executePlay  (beans)
--  where brkdn = breakdown brd
--        slots = getSide brkdn
--        (s, beans) = insideMovement slots pos

breakdown :: Board -> FauxBoard
breakdown Board {goalP1 = g1, slotsP1 = s1, goalP2 = g2, slotsP2 = s2, playerTurn = p} = (g1,s1,g2,s2,p)

getSide :: FauxBoard -> [Slot]
getSide (g1,s1,g2,s2,p) = if p == P1 then s1 else s2

getOtherSide :: FauxBoard -> [Slot]
getOtherSide (g1,s1,g2,s2,p) = if p == P1 then s2 else s1

--Moves the Peices on one side of the board, does not delete the starting peice
--First int is position, scond int is beans
sideMovement :: [Slot] -> Int -> Bean -> [Slot]
sideMovement _ _ 0 = error "Shouldnt Have a zero bean input"
sideMovement slots pos beans = if beans > length midSlots then error "Too many beans"
                               else firstSlots ++ (splitAndRebuild midSlots beans)
  where firstSlots = fst (splitAt (pos - 1) slots)
        midSlots = snd (splitAt (pos - 1) slots)

splitAndRebuild :: [Slot] -> Int -> [Slot]
splitAndRebuild slots 0 = slots
splitAndRebuild (b:bs) beans = (b+1):(splitAndRebuild bs (beans-1))


--Doesnt work for bigger boards than standard
--insideMovement :: [Slot] -> Int -> ([Slot], Bean)
--insideMovement slots pos = newFront front ++ sideMovement back (pos-1) beans
--  where splice = splitAt pos slots
--        front = fst splice
--        back = snd splice
--        beans = last front
--
--newFront :: [Slot] -> [Slot]
--newFront [b] = [0]
--newFront (b:bs) = b:(newFront bs)



executePlay :: Board -> Bean -> Board
executePlay brd pl = undefined
--Board {goalP1 = g1, slotsP1 = s1, goalP2 = g2, slotsP2 = s2, playerTurn = p} pos = if 7 - pos == num then --goes in goal
--       else if 7 - pos < num then  --stays on p1 side
--       else --goes to p2 side

checkCapture :: Board -> Int -> Board
checkCapture brd pos =
  let fauxBrd = breakdown brd
      currentSide = getSide fauxBrd
  in if (currentSide !! (pos - 1)) == 1 then capture brd pos else brd

-- takes a board and a position and return an updated board
capture :: Board -> Int -> Board
capture brd pos =
  let fauxBrd = breakdown brd
      currentSide = getSide fauxBrd
      otherSide = getOtherSide fauxBrd
      goal1 (g1,_,_,_,p) beans = if p == P1 then g1 + beans else g1
      goal2 (_,_,g2,_,p) beans = if p == P2 then g2 + beans else g2
      beanTotal = 1 + (otherSide !! (pos-1))
  in  Board (goal1 fauxBrd beanTotal) (makePosZero (getSide1 fauxBrd) pos) (goal2 fauxBrd beanTotal) (makePosZero (getSide2 fauxBrd) pos) (getPlayer (fauxBrd))

getPlayer :: FauxBoard -> Player
getPlayer (_,_,_,_,p) = p

getSide1 :: FauxBoard -> [Slot]
getSide1 (_,s1,_,_,_) = s1

getSide2 :: FauxBoard -> [Slot]
getSide2 (_,_,_,s2,_) = s2

makePosZero :: [Slot] -> Int -> [Slot]
makePosZero slots pos = frontHalf ++ [0] ++ backHalf
  where frontHalf = tail (fst(splitAt (pos) slots))
        backHalf = snd(splitAt (pos) slots)


-- checks if it is possible to capture a piece and if so changs the board


-- GameState Functions
------------------------------------


updateOutcome :: Board -> Outcome
updateOutcome = undefined

-- takes a board and returns: Turn, Winner, or Tie
















