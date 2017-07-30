module NewMinesweeperTest where

import Test.Hspec
import NewMinesweeper

main :: IO ()
main = hspec $ do
  describe "NewMinesweeper" $ do
  
    it "returns initial Board values" $
      (initialBoard 3) `shouldMatchList` [[Mine 0 0 Closed, Cell 0 1 Closed, Cell 0 2 Closed],
                                          [Cell 1 0 Closed, Mine 1 1 Closed, Cell 1 2 Closed],
                                          [Cell 2 0 Closed, Cell 2 1 Closed, Cell 2 2 Closed]]

    it "get string format of board" $
      (boardStringFormat (initialBoard 3)) `shouldBe` "XXX\nXXX\nXXX\n"


    it "changes the state of cell to opened if user action is open " $
      (applyAction (initialBoard 3) 0 1 0 Open) `shouldMatchList` [[Mine 0 0 Closed, Cell 0 1 Closed, Cell 0 2 Closed],
                                            [Cell 1 0 Opened, Mine 1 1 Closed, Cell 1 2 Closed],
                                            [Cell 2 0 Closed, Cell 2 1 Closed, Cell 2 2 Closed]]


    it "changes the state of mine to blasted if user action is open " $
      (applyAction (initialBoard 3) 0 0 0 Open) `shouldMatchList` [[Mine 0 0 Blasted, Cell 0 1 Closed, Cell 0 2 Closed],
                                            [Cell 1 0 Closed, Mine 1 1 Closed, Cell 1 2 Closed],
                                            [Cell 2 0 Closed, Cell 2 1 Closed, Cell 2 2 Closed]]


    it "changes the state of mine to flagged if user action is flag " $
      (applyAction (initialBoard 3) 0 0 0 Flag) `shouldMatchList` [[Mine 0 0 Flagged, Cell 0 1 Closed, Cell 0 2 Closed],
                                            [Cell 1 0 Closed, Mine 1 1 Closed, Cell 1 2 Closed],
                                            [Cell 2 0 Closed, Cell 2 1 Closed, Cell 2 2 Closed]]


    it "changes the state of cell to flagged if user action is flag " $
      (applyAction (initialBoard 3) 0 0 1 Flag) `shouldMatchList` [[Mine 0 0 Closed, Cell 0 1 Flagged, Cell 0 2 Closed],
                                            [Cell 1 0 Closed, Mine 1 1 Closed, Cell 1 2 Closed],
                                            [Cell 2 0 Closed, Cell 2 1 Closed, Cell 2 2 Closed]]

     
    it "should return the string format of the updated board" $
        (boardStringFormat (applyAction (initialBoard 3) 0 0 1 Flag)) `shouldBe` "XFX\nXXX\nXXX\n"


    it "should return status of the game lost when user opens the mine" $
        gamestatus ((applyAction (initialBoard 3) 0 0 0 Open)) `shouldBe` "Oops, you stepped on a mine ! Game over !"


    it "should return status of the game Continue if the game not over" $
        gamestatus ((applyAction (initialBoard 3) 0 1 0 Open)) `shouldBe` "Continue"


    it "should return status of the game won when user opens the mine" $ do
        let rows = [[Mine 0 0 Flagged, Cell 0 1 Opened, Cell 0 2 Opened],
                    [Cell 1 0 Opened, Mine 1 1 Flagged, Cell 1 2 Opened],
                    [Cell 2 0 Opened, Cell 2 1 Opened, Cell 2 2 Opened]]
        gamestatus (rows) `shouldBe` "Wow, you cleared the minefield ! Game over" 




