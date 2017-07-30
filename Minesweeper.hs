module Minesweeper(
initGrid,
gridStringList,
gridStringFormat,
newGrid,
evaluateGameStatus,
State(Close, Mine, Open, Flag),
Coord(),
Cell(Cell)
) where
import qualified Data.Map as Map
import Data.Map (Map)

data State = Close | Mine | Open | Flag deriving (Eq, Show)
type Coord = (Int,Int)
data Cell = Cell { state :: State, coord :: Coord } deriving Show

mineCells  = [(1,1), (0,0)]

initGrid n = Map.fromList $ [(c, Cell Close c) | x<- [0..n], y<-[0..n], let c = (x,y)]

cellStateStringMatcher Close = "x" 
cellStateStringMatcher Open = "o"
cellStateStringMatcher Mine = "m"
cellStateStringMatcher Flag = "f"

userInputStateMatcher "F" = Flag
userInputStateMatcher "O" = Open

gridStringFormat a [] = "" 
gridStringFormat a (x:xs) = if (a == 0) then ( x ++ "\n" ++ (gridStringFormat 2 xs)) else  (x ++ (gridStringFormat (a-1) xs))

statusForNewGrid c s = if (c `elem` mineCells && s == "O") then Mine else userInputStateMatcher s

gridStringList grid n =  ([ case Map.lookup (x,y) grid of { Just (Cell s _)  -> (cellStateStringMatcher s)} | x <- [0..n], y <- [0..n]] ) 

newGrid grid c s  = Map.insert c (Cell (statusForNewGrid c s) c) grid

count x [] = 0
count x (y:ys) | x==y = 1+(count x ys)
               | otherwise = count x ys

evaluateGameStatus grid n
 | elem "m" (gridStringList grid n) = "Oops, you stepped on a mine ! Game over !" 
 | elem "x" (gridStringList grid n) = "Continue" 
 | (count "m" (gridStringList grid n)) == n = "Wow, you cleared the minefield ! Game over"
 | otherwise = "Continue"

