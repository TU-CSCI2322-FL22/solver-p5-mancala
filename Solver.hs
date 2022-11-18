module Solver where
import Mancala
import Data.Maybe
import Debug.Trace

testBoard =  Board {goalP1 = 2, slotsP1 = [0,0,0,3,0,0], goalP2 = 5, slotsP2 = [0,0,0,1,0,0], playerTurn = P1}
testOutcomes = [Winner P2, Tie]

whoWillWin:: Board -> Outcome
whoWillWin brd@(Board g1 s1 g2 s2 p) =
  case updateOutcome brd of
    Nothing ->
      let moveBoards = traceShowId $  catMaybes [(move brd pos)| pos <- validMoves brd]
          outcomes =  [whoWillWin brd|brd <- moveBoards]
      in traceShow (brd,moveBoards,outcomes) $ bestOutcome outcomes p
    Just outcome -> outcome

playerWins :: [Outcome] -> Player -> [Outcome]
playerWins outcomes p = [outcome|outcome <- outcomes, outcome == Winner p]

playerTies :: [Outcome] -> [Outcome]
playerTies outcomes = [outcome|outcome <- outcomes, outcome == Tie]

bestOutcome :: [Outcome] -> Player -> Outcome
bestOutcome [] _ = error "Umm this shouldnt happen"
bestOutcome outcomes P1 = if playerWins outcomes P1 /= [] then Winner P1 else if playerTies outcomes /= [] then Tie else Winner P2
bestOutcome outcomes P2 = if playerWins outcomes P2 /= [] then Winner P2 else if playerTies outcomes /= [] then Tie else Winner P1



bestMove:: Board -> Board
bestMove brd@(Board g1 s1 g2 s2 p) =
  case updateOutcome brd of
    Nothing ->
      let moveBoards = catMaybes [(move brd pos)| pos <- validMoves brd]
          outcomes =  zip moveBoards [whoWillWin brd|brd <- moveBoards]
          playerWins = filter (\(a,b) -> b == Winner p) outcomes
          playerTies = filter (\(a,b) -> b == Tie) outcomes
      in if length playerWins /= 0 then fst(head playerWins) else if length playerTies /= 0 then fst(head playerTies) else head moveBoards
    Just outcome -> brd
{-
bestMove :: [Board] -> Board
bestMove [] = error "No moves bro"
bestMove brds = snd (maximum [(sum (fst(getSide brd)),brd)|brd <- brds])
-}