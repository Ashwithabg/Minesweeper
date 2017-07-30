module NewMinesweeper (
State(Opened, Closed, Flagged, Blasted),
Cell(Cell,Mine),
Action(Open,Flag), 
initialBoard,
boardStringFormat,
applyAction,
gamestatus
) 
where
-- import Data.List
data Action = Open | Flag deriving (Eq) 
data State  = Opened | Flagged | Closed | Blasted deriving (Eq)
data Cell   = Cell Int Int State | Mine Int Int State deriving (Eq) 


mineSquares = [(0,0), (1,1)] 

initialRow x y 0 = [] 
initialRow x y n   | n == y                     = []
                   | ((x,y) `elem` mineSquares) = (Mine x y Closed) : initialRow x (y+1) n 
                   | otherwise                  = (Cell x y Closed) :  initialRow x (y+1) n

initialBoard n   = [initialRow x 0 n | x <- [0..(n-1)]]

instance Show State where
  show (Opened)  = "O"
  show (Flagged) = "F"
  show (Closed)  = "X"
  show (Blasted) = "M"

instance Show Cell where 
  show (Cell _ _ state) = show state
  show (Mine _ _ state) = show state



rowString []     = ""
rowString (x:xs) = show(x) ++ (rowString xs) 

boardStringFormat  []    = ""
boardStringFormat (x:xs) = (rowString x) ++ "\n" ++ (boardStringFormat xs)

changeState (Cell x y a) Open = Cell x y Opened
changeState (Cell x y a) Flag = Cell x y Flagged
changeState (Mine x y a) Open = Mine x y Blasted
changeState (Mine x y a) Flag = Mine x y Flagged



updateRow [] y n = [] 
updateRow ((Cell a b c) : xs) j n  =  if ( b == j ) then ((changeState (Cell a b c) n ) : xs) else ((Cell a b c) : updateRow xs j n) 
updateRow ((Mine a b c) : xs) j n  =  if ( b == j ) then ((changeState (Mine a b c) n ) : xs) else ((Mine a b c) : updateRow xs j n)

applyAction [] index x y n  = []
applyAction (xs:xxs) index x y value | (index ==  3) = (xs                     : xxs) 
                                     | (x == index)  = ((updateRow xs y value) : xxs)
                                     | otherwise   = xs                      : (applyAction xxs (index+1) x y value )


row []                  = []
row (Cell x y state:xs) =  state : (row xs) 
row (Mine x y state:xs) =  state : (row xs) 

rows []             = []
rows (x:xs)         = (row x) ++ (rows xs)

count x [] = 0
count x (y:ys) | (x == y)   = 1 + (count x ys)
               | otherwise  = count x ys

gamestatus board | (Blasted `elem` (rows board))                                                             = "Oops, you stepped on a mine ! Game over !"
                 | (Closed `notElem` (rows board)) && ((count Flagged (rows board)) == (length mineSquares)) = "Wow, you cleared the minefield ! Game over" 
                 | otherwise                                                                                 = "Continue"


