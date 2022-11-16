

-- "0 1 2 3 4 5 6 0 1 2 3 4 5 6 P1"

readGame :: String -> Board
readGame str = readGameHelp (map (\x -> read x :: Slot)(init split)) (read (last split) :: Player)
        where split = words str

readGameHelp [g1, a, b, c, d, e, f, g2, h, i, j, k, l, m] P1 = Board g1 [a, b, c, d, e, f] g2 [h, i, j, k, l, m] P1
readGameHelp [g1, a, b, c, d, e, f, g2, h, i, j, k, l, m] P2 = Board g1 [a, b, c, d, e, f] g2 [h, i, j, k, l, m] P2
readGameHelp _ _ = error "incorrect string"


showGame :: Board -> String
showGame (Board g1 lst g2 lst2 P1) = concat [(show g1), " ", showGameHelp lst, (show g2), " ", showGameHelp lst, (show P1)]


showGameHelp [] = []
showGameHelp (x:xs) = concat[(show x), " ", showGameHelp xs]
