import Minesweeper
import Evaluate
import System.Exit

main = do
let grid = initGrid 2
putStrLn "Welcome to Minesweeper game...."
putStrLn "-------------------------------------------------"
putStrLn $ (gridStringFormat 2 (gridStringList grid 2))
putStrLn "-------------------------------------------------"
evaluate grid


