

-- "0 1 2 3 4 5 6 0 1 2 3 4 5 6 P1"

readGame :: String -> Board
readGame str = readGameHelp (map (\x -> read x :: Slot)(init split)) (read (last split) :: Player)
        where split = words str

readGameHelp [g1, a, b, c, d, e, f, g2, h, i, j, k, l, m] P1 = Board g1 [a, b, c, d, e, f] g2 [h, i, j, k, l, m] P1
readGameHelp [g1, a, b, c, d, e, f, g2, h, i, j, k, l, m] P2 = Board g1 [a, b, c, d, e, f] g2 [h, i, j, k, l, m] P2
readGameHelp _ _ = error "incorrect string"

