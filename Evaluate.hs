module Evaluate (
evaluate
) where
import Minesweeper
import Data.Char


evaluate grid = do 
putStrLn "x and y coordinate: (x,y) :";
coord <- getLine
let c = read coord :: (Int,Int)

putStrLn "Choose the operation\nO = Open the cell\nF = flag for mine\nYour option:"
state <- getLine
let s = map toUpper state

let newGridData = (newGrid grid c s)
putStrLn "-------------------------------------------------"
putStrLn $ (gridStringFormat 2 (gridStringList newGridData 2))
putStrLn "-------------------------------------------------"

let status = (evaluateGameStatus newGridData 2)
if (status == "Continue") then  (evaluate newGridData) else (putStrLn status)

