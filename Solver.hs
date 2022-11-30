module Solver where
import Mancala
import Data.Maybe
import Data.Ratio
import Debug.Trace

testBoard = Board {goalP1 = 0, slotsP1 = [0,0,1,0,0,0], goalP2 = 0, slotsP2 = [0,0,0,0,2,6], playerTurn = P1}
--This Case gives an error
errorBoard =  Board {goalP1 = 1, slotsP1 = [4,4,4,0,5,5], goalP2 = 1, slotsP2 = [5,4,4,4,4,4], playerTurn = P1}
testOutcomes = [Winner P2, Tie]

whoWillWin:: Board -> Outcome
whoWillWin brd@(Board g1 s1 g2 s2 p) =
  case updateOutcome brd of
    Nothing ->
      let moveBoards = {-traceShowId $-}  catMaybes [(move brd pos)| pos <- validMoves brd]
          outcomes =  [whoWillWin brd|brd <- moveBoards]
      in {-traceShow (brd,moveBoards,outcomes)-} bestOutcome outcomes p
    Just outcome -> outcome

--Takes a list of outcomes and a player, returns all Wins for the current player of the game
playerWins :: [Outcome] -> Player -> [Outcome]
playerWins outcomes p = [outcome|outcome <- outcomes, outcome == Winner p]

--Takes a lit of outcomes and returns all Ties
playerTies :: [Outcome] -> [Outcome]
playerTies outcomes = [outcome|outcome <- outcomes, outcome == Tie]

--takes a list of outcomes and a player and returns the best outcome for the current player
bestOutcome :: [Outcome] -> Player -> Outcome
bestOutcome [] _ = error "Umm this shouldnt happen"
bestOutcome outcomes P1 = if Winner P1 `elem` (playerWins outcomes P1) then Winner P1 else if playerTies outcomes /= [] then Tie else Winner P2
bestOutcome outcomes P2 = if playerWins outcomes P2 /= [] then Winner P2 else if playerTies outcomes /= [] then Tie else Winner P1

maybeMover :: (Maybe a, b) -> Maybe (a, b)
maybeMover (a, b) = fmap (\q -> (q, b)) a

-- Takes a board and gibes the best move for the current player
bestMove:: Board -> Int
bestMove brd@(Board g1 s1 g2 s2 p) =
  case updateOutcome brd of
    Nothing ->
      let moveBoards = catMaybes [ maybeMover ((move brd pos),pos)| pos <- validMoves brd]
          outcomes =  [(whoWillWin (fst brd),brd)|brd <- moveBoards]
          playerWins = filter (\(a,b) -> a == Winner p) outcomes
          playerTies = filter (\(a,b) -> a == Tie) outcomes
      in if lookup (Winner p) playerWins /= Nothing then snd (snd(head playerWins)) else if lookup Tie playerTies /= Nothing then snd (snd(head playerTies)) else snd (head moveBoards)
    Just outcome -> error "Already won the game, no moves possible"

bestMoveBounded:: Board -> Int -> Int
bestMoveBounded brd@(Board g1 s1 g2 s2 p) 0 = getBoardState brd
bestMoveBounded brd@(Board g1 s1 g2 s2 p) depth=
      let moveBoards = catMaybes [ maybeMover ((move brd pos),pos)| pos <- validMoves brd]
          outcomes =  [(bestMoveBounded (fst brd) depth -1,brd)|brd <- moveBoards]
          bestScore = [(getBoardState brd, mve)|(_,(brd,mve)) <- outcomes]
      in snd (maximum bestScore)

getBoardState :: Board -> Int
getBoardState (Board g1 s1 _ _ P1) = ( g1) + sum s1
getBoardState (Board _ _ g2 s2 P2) = ( g2) + sum s2