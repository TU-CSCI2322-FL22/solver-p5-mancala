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

--Board {goalP1 = g1, slotsP1 = s1, goalP2 = g2, slotsP2 = s2, playerTurn = p}
-- pattern match and return the opposite slot for every position
move :: Board -> Int -> Board
move brd pos =
  let (g1,s1,g2,s2,p) = breakdown brd
      (slots,x) = getSide (g1,s1,g2,s2,p)
      (s, beans) = insideMovement slots pos
  in if x == 2 then executePlay (g1,s,g2,s2,p) beans 1 else executePlay (g1,s1,g2,s,p) beans 3




breakdown :: Board -> FauxBoard
breakdown Board {goalP1 = g1, slotsP1 = s1, goalP2 = g2, slotsP2 = s2, playerTurn = p} = (g1,s1,g2,s2,p)



getSide :: FauxBoard -> ([Slot],Int)
getSide (g1,s1,g2,s2,p) = if p == P1 then (s1,2) else (s2,4)



--Moves the Peices on one side of the board, does not delete the starting peice
--First int is position, scond int is beans
sideMovement :: [Slot] -> Int -> Bean -> [Slot]
sideMovement _ _ 0 = error "Shouldnt Have a zero bean input"
sideMovement slots pos beans = splitAndRebuild slots beans



splitAndRebuild :: [Slot] -> Bean -> [Slot]
splitAndRebuild slots 0 = slots
splitAndRebuild [] x = []
splitAndRebuild (b:bs) beans = (b+1):(splitAndRebuild bs (beans-1))



--Doesnt work for bigger boards than standard
insideMovement :: [Slot] -> Int -> ([Slot],Bean)
insideMovement slots pos = (newFront front ++ sideMovement back (pos-1) beans, beans-(6-pos))
  where splice = splitAt pos slots
        front = fst splice
        back = snd splice
        beans = last front



newFront :: [Slot] -> [Slot]
newFront [b] = [0]
newFront (b:bs) = b:(newFront bs)



executePlay :: FauxBoard -> Bean -> Int -> Board
executePlay (g1,s1,g2,s2,p) beans 1 = if beans > 0 then if p == P1 then executePlay (g1+1,s1,g2,s2,p) (beans-1) 4 else executePlay (g1,s1,g2,s2,p) (beans) 4 else Board g1 s1 g2 s2 p
executePlay (g1,s1,g2,s2,p) beans 2 = if beans > 0 then executePlay (g1,sideMovement s1 1 beans,g2,s2,p) (beans-6) 1 else Board g1 s1 g2 s2 p
executePlay (g1,s1,g2,s2,p) beans 3 = if beans > 0 then if p == P2 then executePlay (g1,s1,g2+1,s2,p) (beans-1) 2 else executePlay (g1,s1,g2,s2,p) (beans) 2 else Board g1 s1 g2 s2 p
executePlay (g1,s1,g2,s2,p) beans 4 = if beans > 0 then executePlay (g1,s1,g2,sideMovement s2 1 beans,p) (beans-6) 3 else Board g1 s1 g2 s2 p
--Board {goalP1 = g1, slotsP1 = s1, goalP2 = g2, slotsP2 = s2, playerTurn = p} pos = if 7 - pos == num then --goes in goal
--       else if 7 - pos < num then  --stays on p1 side
--       else --goes to p2 side

checkCapture :: Board -> Bool
checkCapture = undefined

-- takes a board and a position and return an updated board
capture :: Board -> Board
capture = undefined

-- checks if it is possible to capture a piece and if so changs the board


-- GameState Functions
------------------------------------


updateOutcome :: Board -> Outcome
updateOutcome = undefined

-- takes a board and returns: Turn, Winner, or Tie
















