module MinesweeperTest where

import Test.Hspec
import Minesweeper
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = hspec $ do
  describe "Minesweeper" $ do
  
    it "returns initial grid Cell values" $
      (gridStringList (initGrid 0) 0) `shouldBe` ["x"]

    it "changes the state of the cell to open" $
      gridStringList (newGrid (initGrid 1) (1, 0) "O") 1 `shouldBe` ["x","x","o","x"]

    it "changes the state of the cell to flag" $
      gridStringList (newGrid (initGrid 1) (0, 0) "F") 1 `shouldBe` ["f","x","x","x"]

    it "changes the state of the cell to Mine" $
      gridStringList (newGrid (initGrid 1) (0, 0) "O") 1 `shouldBe` ["m","x","x","x"]

    it "gives the grid string format to display" $
      gridStringFormat 2 ["x","x","x","x","x","x","x","x","x"] `shouldBe` "xxx\nxxx\nxxx\n"

    it "does not change the status if the game is not won or lost" $ 
      evaluateGameStatus (newGrid (initGrid 1) (0, 0) "F") 1 `shouldBe` "Continue"

    it "changes the status of the game when mine is opened" $
      evaluateGameStatus (newGrid (initGrid 1) (0, 0) "O") 1 `shouldBe` "Oops, you stepped on a mine ! Game over !"

    it "changes the status of the game when game is won" $ 
      evaluateGameStatus (newGrid (initGrid 0) (0, 0) "F") 0 `shouldBe` "Wow, you cleared the minefield ! Game over"

    it "does not change the status if the flag is applied to non mined place" $ do
      let grid = Map.fromList $ [(c, Cell Flag c) | x<- [0..1], y<-[0..1], let c = (x,y)]
      evaluateGameStatus (newGrid grid (0, 0) "F") 1 `shouldBe` "Continue"
