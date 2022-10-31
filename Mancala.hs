data Slot = Small (Int, Type1) | Goal (Int, Type2 ) deriving (Show, Eq)

data Type1 = Int Int deriving Show
data Type2 = Int deriving Show

data Turn = Check Turn deriving (Show, Eq)

board = [Slot 0, Slot (1,13), Slot (, Slot Small, Slot Small, Slot Small, Slot Small, Slot Goal, Slot Small, Slot Small, Slot Small, Slot Small, Slot Small, Slot Small]


