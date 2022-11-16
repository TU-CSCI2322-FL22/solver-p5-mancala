module IO where

import "Mancala.hs"


readGame :: String -> Board

showGame :: Board -> String

writeGame :: Board -> FilePath -> IO ()
writeGame brd path = writeFile path (showGame brd)

loadGame :: FilePath -> IO Board
loadGame path = 
  do brd = readFile path
     return (readGame brd)

-- can complete when the algorithm is complete
putWinner :: Board -> IO ()
putWinner brd = return getWinner brd


