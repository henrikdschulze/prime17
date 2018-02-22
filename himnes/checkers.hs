import Data.Char
import Control.Monad
import Control.Exception
import Debug.Trace
import Data.List


{- the Player is represented by a color and a type
   INVARIANT:
     There are only three colors and three types because Player None NotHere is there to represent if there is no piece in that position, and that Color None and Type NotHere is
     there as an edge case. 
     Each Color corresponds to a character and uppercase or lowercase to represent the character's type. 
     Player Red Normal & Player Red Queen represent Player Red while Player White Normal & Player White Queen represent Player White. 
 -}
data Player = Player Color Type

{- the Color is either Red, White or None. 
   INVARIANT:
     There are only three colors and each Color corresponds to a character. "r" for Red and "w" for White. These will represent the colors of the pieces
     one the board. Color must be represented as a String. None is a ".", meaning there is no piece there.  
 -}
data Color = Red | White | None

{- the Type is either Normal, Queen or NotHere.
    INVARIANT:
     There are only three types. Lowercase characters represent "Normal" pieces and uppercase characters represent "Queen" pieces, meaning for example that
     player Red have managed to move one of the pieces to the last row. "r" for Red Normal, "R" for Red Queen and "w" for White Normal and "W" for White Queen. 
     NotHere means that the space is empty, namely ".". 
-}
data Type = Normal | Queen | NotHere

{- GameState is the board and is represented as a list of strings in a list. 
   INVARIANT: The GameState is never to be empty and should always be 8x8 in size. No numbers should be in the GameState. 
 -}
type GameState = [[String]]

{- GameStateWithPosition is the board with positions, where the position is represented as a tuple with the
    first number being the row number, and the second number being the column number. 
   INVARIANT: The GameState is never to be empty and should always be 8x8 in size. The positions can't be Floats.  
 -}
type GameStateWithPosition = [[(String,Position)]]

{- ListStateWithPosition is the a concatenation of the board with positions. This version is needed to find specific elements in the list.  
   INVARIANT: The positions can't be Floats. The ListwithPosition should always have the same number of elements as the GameState.   
 -}
type ListwithPosition = [(String,Position)]

{- List is a list of strings. It's basically the board but with every element in one row instead. 
   INVARIANT: Only Strings, also the list should never be empty.  
 -}
type List = [String]

{- The Position is represented as a tuple with the first number being the row number and the second number
    being the column number. 
   INVARIANT: row number are 1-8. Column numbers are 1-8. No numbers outside these are valid. 
 -}
type Position = (Int, Int)

{- Move is represented as a tuple with the first number being the row number and the second number
    being the column number. 
   INVARIANT: row number are 1-8. Column numbers are 1-8. No numbers outside these are valid.
 -}
type Move = (Int, Int)

{- readPiece Player
    Converts the Player data type to a String. 
    PRE:     True
    RETURNS: String
    EXAMPLES: readPiece (Player Red Normal) = "r"
              readPiece (Player White Normal) = "w"
-}
readPiece:: Player -> String
readPiece (Player Red Normal) = "r"
readPiece (Player Red Queen) = "R"
readPiece (Player White Normal) = "w"
readPiece (Player White Queen) = "W"
readPiece (Player None NotHere) = "."

{- showPiece String
    Takes a string and returns the same string. Useful when you are not sure 
    what string it is in some position in the gamestate.  
    PRE:     True
    RETURNS: String
    EXAMPLES: showPiece "w" = "w"
              showPiece "r" = "r"
-}
showPiece :: String -> String
showPiece "w" = readPiece (Player White Normal)
showPiece "W" = readPiece (Player White Queen)
showPiece "r" = readPiece (Player Red Normal)
showPiece "R" = readPiece (Player Red Queen)
showPiece "." = readPiece (Player None NotHere)

{- upgradePiece String
    Takes a string and returns the string in uppercase. If the string already is in uppercase,
    nothing happens, it just returns the same string. 
    PRE:     True
    RETURNS: String
    EXAMPLES: upgradePiece "r" = "R"
              upgradePiece "w" = "W"
-}
upgradePiece:: String -> String 
upgradePiece "r" = showPiece "R"
upgradePiece "w" = showPiece "W"
upgradePiece "W" = showPiece "W"
upgradePiece "R" = showPiece "R"


{- emptyBoard
    Makes a "empty board" with 64 elements. It's filled with "." to signify that there are no pieces at that position. 
    RETURNS: List
    EXAMPLES: emptyBoard = [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."
                            ,".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",
                            ".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."]
-}
emptyBoard::List
emptyBoard = replicate 64 "."

{- addPosition
    adds positions to the elements in the list. Row number is the first number and goes from 1 to 8. Column number is the second number and goes from 1 to 8. 
    RETURNS: ListwithPosition
    EXAMPLES: addPosition = [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))]
-}
addPosition::ListwithPosition
addPosition = let (x:xs) = insertPlayer in
                  addPositionAux (x:xs) (1,1) where
{- addPositionAux List Position
    adds positions to the elements in the list. Row number is the first number and goes from 1 to 8. Column number is the second number and goes from 1 to 8. 
    PRE: True
    RETURNS: ListwithPosition
    EXAMPLES: addPositionAux [] _ = []
              addPositionAux ["r","."] (1,1) = [("r",(1,1)),(".",(1,2))]
-}
                      addPositionAux:: List -> Position -> ListwithPosition
                      addPositionAux [] _ = []
                      addPositionAux all@(x:xs) (a,b)
                          |b < 8 = [(x,(a,b))] ++ addPositionAux xs (a,b+1)
                          |b == 8 = [(x,(a,b))] ++ addPositionAux xs (a+1,b-7)

{- removePosition ListwithPosition
    removes the positions from the list, returning only a list with the empty tiles and pieces.  
    PRE: True
    RETURNS: List
    EXAMPLES: removePosition [] = []
              removePosition [("r",(1,1)),(".",(1,2))] = ["r","."]
-}                          
removePosition :: ListwithPosition -> List
removePosition [] = []
removePosition ((x,(a,b)):xs) = [x] ++ removePosition xs

{- addPositionGameState GameState
    adds positions to the GameState. Row number is the first number and goes from 1 to 8. Column number is the second number and goes from 1 to 8. 
    PRE: No empty list
    RETURNS: GameStateWithPosition
    EXAMPLES: addPositionGameState [["r","."],["r"]] = [[("r",(1,1)),(".",(1,2)),("r",(1,3))]]

-}
addPositionGameState :: GameState -> GameStateWithPosition
addPositionGameState (x:xs) = makeGamestate (addPositionAux (concat(x:xs)) (1,1)) where
{- addPositionAux List Position
    adds positions to the elements in the list. Row number is the first number and goes from 1 to 8. Column number is the second number and goes from 1 to 8. 
    PRE: True
    RETURNS: ListwithPosition
    EXAMPLES: addPositionAux [] _ = []
              addPositionAux ["r","."] (1,1) = [("r",(1,1)),(".",(1,2))]
-}
    addPositionAux:: List -> Position -> ListwithPosition 
    addPositionAux [] _ = []
    addPositionAux all@(x:xs) (a,b)
        |b < 8 = [(x,(a,b))] ++ addPositionAux xs (a,b+1)
        |b == 8 = [(x,(a,b))] ++ addPositionAux xs (a+1,b-7)


{- insertPlayer
    Insert the player pieces into the empty list.  
    RETURNS: List
    EXAMPLES: insertPlayer = ["r",".","r",".","r",".","r",".",".","r",".",
                            "r",".","r",".","r","r",".","r",".","r",".","r",
                            ".",".",".",".",".",".",".",".",".",".",".",".",
                            ".",".",".",".",".",".","w",".","w",".","w",".",
                            "w","w",".","w",".","w",".","w",".",".","w",".",
                            "w",".","w",".","w"]
-}
insertPlayer::List
insertPlayer = let (x:y:xs) = emptyBoard in insertPlayerRed (x:y:xs) readPiece (Player Red Normal) (1) where
{- insertPlayerRed List readPiece Player Int
    Insert the player Red pieces into the empty list as strings. After that it calls the insertPlayerWhite function and concatinates itself with that list. 
    Pre: List must have an even number of elements
    RETURNS: List
    EXAMPLES: insertPlayerRed emptyBoard readPiece (Player Red Normal) 1 = ["r",".","r",".","r",".","r",".",".","r",".","r",".","r",".","r","r",".","r",".","r",".","r",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","w",".","w",".","w",".","w","w",".","w",".","w",".","w",".",".","w",".","w",".","w",".","w"]
-}
    insertPlayerRed :: (Ord a, Num a) => [String] -> (Player -> String) -> Player -> a -> [String]
    insertPlayerRed [] _ _ _ = []
    insertPlayerRed (x:y:xs) readPiece (Player Red Normal) (num)
        |num <= 4 = (readPiece (Player Red Normal)):y:(insertPlayerRed xs readPiece (Player Red Normal) (num+1))
        |num > 4 && num <= 8 = y:(readPiece (Player Red Normal)):(insertPlayerRed xs readPiece (Player Red Normal) (num+1))
        |num > 8 && num < 12 = (readPiece (Player Red Normal)):y:(insertPlayerRed xs readPiece (Player Red Normal) (num+1))
        |num == 12 = (readPiece (Player Red Normal)):y:(reverse(insertPlayerWhite (reverse xs)))
 
{- insertPlayerWhite List
    Insert the player White pieces into a list.
    Pre: List must have an even number of elements
    RETURNS: List
    EXAMPLES: insertPlayerWhite ["r",".","r"] = ["w","."*** Exception: checkers.hs:(203,5)-(208,59): Non-exhaustive patterns in function insertPlayerWhiteAux
              insertPlayerWhite ["r",".","r","."] = ["w",".","w","."]
-} 
insertPlayerWhite:: List -> List
insertPlayerWhite xs = insertPlayerWhiteAux xs readPiece (Player White Normal) (1) where
{- insertPlayerWhiteAux List readPiece Player Int
    Insert the player White pieces into the empty list as strings. 
    PRE: List must have an even number of elements
    RETURNS: List
    EXAMPLES: insertPlayerWhiteAux emptyBoard readPiece (Player White Normal) 1 = ["w",".","w",".","w",".","w",".",".","w",".","w",".","w",".","w","w",".","w",".","w",".","w",".",".",".",
    ".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."]
-}
    insertPlayerWhiteAux :: (Ord a, Num a) => [String] -> (Player -> String) -> Player -> a -> [String]
    insertPlayerWhiteAux [] _ _ _ = []
    insertPlayerWhiteAux (x:y:ys) readPiece (Player White Normal) (num)
        |num <= 4 = (readPiece (Player White Normal)):y:(insertPlayerWhiteAux ys readPiece (Player White Normal) (num+1))
        |num > 4 && num <= 8 = y:(readPiece (Player White Normal)):(insertPlayerWhiteAux ys readPiece (Player White Normal) (num+1))
        |num > 8 && num < 12 = (readPiece (Player White Normal)):y:(insertPlayerWhiteAux ys readPiece (Player White Normal) (num+1))
        |num == 12 = (readPiece (Player White Normal)):y:ys      

{- makeGamestate [a]
    Makes a list into a list of lists
    PRE: No empty list
    RETURNS: [[a]]
    EXAMPLES: makeGamestate ["r",".",".",".",".",".",".",".","."] = [["r",".",".",".",".",".",".","."],["."]]
-}
makeGamestate:: [a] -> [[a]]
makeGamestate (x:xs) = rows (x:xs) where
    rows l@(x:xs) = [take 8 l] ++ rows (drop 8 l)
    rows [] = [] 

{- genGameState
   Purpose: Generate the starting game state
   Pre: None
   Return: IO GameState
   Side effect: None
-}
genGameState:: IO GameState
genGameState = do
    return (makeGamestate (insertPlayer))    

{- main
   Purpose: Starts the game and generates the gameState
   Pre: None
   Return: IO ()
   Side effect: Prints "Welcome to Checkers" on the screen
-}
main :: IO ()
main = do 
  putStrLn "Welcome to Checkers."
  gameState <- genGameState
  play gameState                      

{- play
   Purpose: Prints out the current gamestate, generates new gamestates from the moves from both players,  and checks victory conditions
   Pre: Valid gameState
   Return: IO ()
   Side effect: Prints out gameState on screen. If victory is achieved then prints out message to player. 
-}
play :: GameState -> IO ()
play gameState = do
    printboard gameState
    newGameState <- playerMoveRed gameState
    if victoryRed newGameState then do
        putStrLn "Player Red won!"
        putStrLn ""
        quitPlease
    else do
        newNewGameState <- playerMoveWhite newGameState
        if victoryWhite newNewGameState then do
            putStrLn "Player White won!"
            putStrLn ""
            quitPlease
                                        else
                                        play newNewGameState 
      

       
{- checkifcanUpgradeRed Position
    Checks if the last move caused the red piece to land on row 8, making it valid to become a Queen piece. 
    PRE: True
    RETURNS: Bool
    EXAMPLES: checkifcanUpgradeRed (8,1) = True
              checkifcanUpgradeRed (5,5) = False
-}      
checkifcanUpgradeRed :: Position -> Bool       
checkifcanUpgradeRed (a,b)
    |a == 8 = True
    |otherwise = False
 

 {- checkifcanUpgradeWhite Position
    Checks if the last move caused the white piece to land on row 1, making it valid to become a Queen piece. 
    PRE: True
    RETURNS: Bool
    EXAMPLES: checkifcanUpgradeWhite (1,1) = True
              checkifcanUpgradeWhite (4,5) = False
-}
checkifcanUpgradeWhite :: Position -> Bool
checkifcanUpgradeWhite (a,b)
    |a == 1 = True
    |otherwise = False

 {- upgradeRed GameState Position
    upgrade the red piece to queen. Checks one more time that it is at the correct position and finds the piece corresponding to that position.  
    PRE: No empty list and valid Position. The list must be as large as the position you're checking it against because of !!
    RETURNS: GameState
    EXAMPLES: upgradeRed [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","r"]] (8,8) = [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","R"]]   
-}
upgradeRed :: GameState -> Position -> GameState
upgradeRed newGameState (a,b) = makeGamestate (removePosition (upgradeAux (concat (addPositionGameState newGameState)) (a,b))) where
{- upgradeAux ListwithPosition Position
    upgrade the red piece to queen. Checks one more time that it is at the correct position and finds the piece corresponding to that position.  
    PRE: No empty list and valid Position. The list must be as large as the position you're checking it against because of !!
    RETURNS: ListwithPosition
    EXAMPLES: upgradeAux [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("r",(8,8))] (8,8) = 
    [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("R",(8,8))]
-}
    upgradeAux :: ListwithPosition -> Position -> ListwithPosition
    upgradeAux ((y,(c,d)):ys) (a,b) = let (r,(s,t)) = ((y,(c,d)):ys) !! (findPosition (a,b)) in
                                                 checkUpgrade ((y,(c,d)):ys) (r,(s,t)) where
{- checkUpgrade ListwithPosition (String,Position)
    upgrade the red piece to queen. Checks one more time that it is at the correct position. 
    PRE: No empty list and valid Position. 
    RETURNS: ListwithPosition
    EXAMPLES: checkUpgrade [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("r",(8,8))] ("r,"(8,8)) = 
    [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("R",(8,8))]
-}

                                                     checkUpgrade:: ListwithPosition -> (String,Position) -> ListwithPosition
                                                     checkUpgrade all@((y,(c,d)):ys) (r,(s,t))
                                                         |s == 8 = insertAt ((upgradePiece r),(s,t)) (delete (r,(s,t)) all)
                                                         |otherwise = ((y,(c,d)):ys)
 
{- upgradeWhite GameState Position
    upgrade the white piece to queen. Checks one more time that it is at the correct position and finds the piece corresponding to that position.  
    PRE: No empty list and valid Position. The list must be as large as the position you're checking it against because of !!
    RETURNS: GameState
    EXAMPLES: upgradeWhite [["w","."]] (1,1) = [["W","."]]    
-}
upgradeWhite:: GameState -> Position -> GameState
upgradeWhite newGameState (a,b) = makeGamestate (removePosition (upgradeAux (concat (addPositionGameState newGameState)) (a,b))) where
{- upgradeAux ListwithPosition Position
    upgrade the white piece to queen. Checks one more time that it is at the correct position and finds the piece corresponding to that position.  
    PRE: No empty list and valid Position. The list must be as large as the position you're checking it against because of !!
    RETURNS: ListwithPosition
    EXAMPLES: upgradeAux ["w","."] (1,1) = ["W","."]
-}
    upgradeAux :: ListwithPosition -> Position -> ListwithPosition
    upgradeAux ((y,(c,d)):ys) (a,b) = let (r,(s,t)) = ((y,(c,d)):ys) !! (findPosition (a,b)) in
                                                 checkUpgrade ((y,(c,d)):ys) (r,(s,t)) where
{- checkUpgrade ListwithPosition (String,Position)
    upgrade the white piece to queen. Checks one more time that it is at the correct position. 
    PRE: No empty list and valid Position. 
    RETURNS: ListwithPosition
    EXAMPLES: checkUpgrade [("w",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("r",(8,8))] ("w,"(1,1)) = 
    [("W",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("R",(8,8))]
-}
                                                     checkUpgrade:: ListwithPosition -> (String,Position) -> ListwithPosition
                                                     checkUpgrade all@((y,(c,d)):ys) (r,(s,t))
                                                         |s == 1 = insertAt ((upgradePiece r),(s,t)) (delete (r,(s,t)) all)
                                                         |otherwise = ((y,(c,d)):ys)       


{- playerMoveRed GameState
   Purpose: From the current gamestate, gets input from the player for one position the player want to move from and one position to move to and prints these to the screen.
   Prints out the current gamestate for the player to see. Also checks if certain conditions are met and if so, does those functions. 
   Pre: Valid gameState
   Return: IO GameState
   Side effect: Prints out different gameStates to the screen depending on conditions.
-}
playerMoveRed :: GameState -> IO GameState
playerMoveRed gameState = do
  putStrLn "Player Red move from"
  move1 <- readMove
  putStrLn "Player Red move to"
  move2 <- readMove
  printMove "Player Red" move1 move2
  if (validchoiceRed gameState move1 && validMoveRed gameState move1 move2) then do
      newGameState <- return (playMove gameState move1 move2)
      if checkifcanUpgradeRed move2 then do
          newsGameState <- return (upgradeRed newGameState move2)
          printboard newsGameState
          return $ newsGameState  
                                                     else do
                                                         printboard newGameState
                                                         if checkPosition newGameState move1 move2 then do
                                                             doubleMoveRed newGameState move2 else
                                                             return $ playMove gameState move1 move2 
    else do
        putStrLn "Invalid Move. You can only move your own pieces and move diagonally"
        playerMoveRed gameState             
            
{- playerMoveWhite GameState
   Purpose: From the current gamestate, gets input from the player for one position the player want to move from and one position to move to and prints these to the screen.
   Prints out the current gamestate for the player to see. Also checks if certain conditions are met and if so, does those functions. 
   Pre: Valid gameState
   Return: IO GameState
   Side effect: Prints out different gameStates to the screen depending on conditions.
-}            
playerMoveWhite :: GameState -> IO GameState            
playerMoveWhite gameState = do
  putStrLn "Player White move from"
  move1 <- readMove
  putStrLn "Player White move to"
  move2 <- readMove
  printMove "Player White" move1 move2
  if (validchoiceWhite gameState move1 && validMoveWhite gameState move1 move2) then do
      newGameState <- return (playMove gameState move1 move2)
      if checkifcanUpgradeWhite move2 then do 
          newsGameState <- return (upgradeWhite newGameState move2)
          return $ newsGameState 
                                                          else do
                                                              if checkPositionwhite newGameState move1 move2 then do
                                                                  printboard newGameState
                                                                  doubleMoveWhite newGameState move2 else do
                                                                      return $ playMove gameState move1 move2
   else do
       putStrLn "Invalid Move. You can only move your own pieces and move diagonally"
       playerMoveWhite gameState       


{- doubleMoveWhite GameState Move
   Purpose: If after a jump, the player is able to jump again, this functions starts forcing the player to move the same piece.
   Pre: Valid gameState and valid Move
   Return: IO GameState
   Side effect: Prints out different gameStates to the screen depending on conditions and returns the gamestate to the playerMoveWhite function. 
-}
doubleMoveWhite:: GameState -> Move -> IO GameState       
doubleMoveWhite newGameState (a,b) = do
    let move3 = (a,b)
    putStrLn ("Player White have to move from  " ++ (show a) ++ " , " ++ (show b))
    putStrLn "Player White move to"
    move4 <- readMove
    printMove "Player White" move3 move4
    if (validchoiceWhite newGameState move3 && validMoveWhite newGameState move3 move4) then do
    newnewGameState <- return (playMove newGameState move3 move4)
    printboard newnewGameState
    if checkifcanUpgradeWhite move4 then do 
    newsGameState <- return (upgradeWhite newnewGameState move4)
    printboard newsGameState
    return $ newsGameState
                                                          else do
    if checkPositionwhite newnewGameState move3 move4 then do doubleMoveWhite newnewGameState move4 else return $ playMove newGameState move3 move4
     else do 
         putStrLn "Invalid Move. You can only move your own pieces and move diagonally"
         doubleMoveWhite newGameState (a,b)       

{- doubleMoveRed GameState Move
   Purpose: If after a jump, the player is able to jump again, this functions starts forcing the player to move the same piece.
   Pre: Valid gameState and valid Move
   Return: IO GameState
   Side effect: Prints out different gameStates to the screen depending on conditions and returns the gamestate to the playerMoveRed function. 
-}
doubleMoveRed:: GameState -> Move -> IO GameState       
doubleMoveRed newGameState (a,b) = do
    let move3 = (a,b)
    putStrLn ("Player Red have to move from  " ++ (show a) ++ " , " ++ (show b))
    putStrLn "Player Red move to"
    move4 <- readMove
    printMove "Player Red" move3 move4
    if (validchoiceRed newGameState move3 && validMoveRed newGameState move3 move4) then do
    newnewGameState <- return (playMove newGameState move3 move4)
    if checkifcanUpgradeRed move4 then do 
    newsGameState <- return (upgradeRed newnewGameState move4)
    printboard newsGameState
    return $ newsGameState
                                               else do
    printboard newnewGameState
    if checkPosition newnewGameState move3 move4 then do doubleMoveRed newnewGameState move4 else return $ playMove newGameState move3 move4
     else do 
         putStrLn "Invalid Move. You can only move your own pieces and move diagonally"
         doubleMoveRed newGameState (a,b)         
         
 
{- readMove
   Purpose: Reads a move from standard input
   Post: A move object 
   Side-effects: Reads one or more lines from standard input 
-}
readMove :: (IO Move) -- reads input from
readMove = do
  catch (do
    line <- getLine
    evaluate (read line))  -- evaluate required to force conversion of line to Move
    ((\_ -> do   -- exception handler
    putStrLn "Invalid input. Correct format: (row,column)" 
    readMove) :: SomeException -> IO Move)
    
    

{- validMoveRed GameState Move Move
    Returns True if the moves are valid for Player Red, returns false otherwise. First move is the position the piece moves from. Second move it the position
    the player wants to move to. 
    PRE: No empty gamestate
    RETURNS: Bool
    EXAMPLES: validMoveRed [["r",".","r",".","r",".","r","."],
                            [".","r",".","r",".","r",".","r"],
                            ["r",".","r",".","r",".","r","."],
                            [".",".",".",".",".",".",".","."],
                            [".",".",".",".",".",".",".","."],
                            [".","w",".","w",".","w",".","w"],
                            ["w",".","w",".","w",".","w","."],
                            [".","w",".","w",".","w",".","w"]] (3,3) (4,4) = True    
-}
validMoveRed :: GameState -> Move -> Move -> Bool
validMoveRed (x:xs) (u,v) (w,q) = validMoveauxRed (concat (addPositionGameState (x:xs))) (u,v) (w,q)

{- validMoveWhite GameState Move Move
    Returns True if the moves are valid for Player White, returns false otherwise. First move is the position the piece moves from. Second move it the position
    the player wants to move to. 
    PRE: No empty gamestate
    RETURNS: Bool
    EXAMPLES: validMoveWhite[["r",".","r",".","r",".","r","."],
                            [".","r",".","r",".","r",".","r"],
                            ["r",".","r",".","r",".","r","."],
                            [".",".",".",".",".",".",".","."],
                            [".",".",".",".",".",".",".","."],
                            [".","w",".","w",".","w",".","w"],
                            ["w",".","w",".","w",".","w","."],
                            [".","w",".","w",".","w",".","w"]] (6,6) (5,5) = True    
-}
validMoveWhite :: GameState -> Move -> Move -> Bool
validMoveWhite (x:xs) (u,v) (w,q) = validMoveauxWhite (concat (addPositionGameState (x:xs))) (u,v) (w,q)

{- validMoveauxRed ListwithPosition Move Move
    Returns True if the moves are valid for Player Red, returns false otherwise. First move is the position the piece moves from. Second move it the position
    the player wants to move to. 
    PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
    RETURNS: Bool
    EXAMPLES: validMoveauxRed [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",    (2,8)),("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))] (3,3) (4,4) = True
              validMoveauxRed [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))] (3,3) (5,5) = False
    
-}
validMoveauxRed ::ListwithPosition -> Move -> Move -> Bool
validMoveauxRed [] _ _ = False
validMoveauxRed ys (u,v) (w,q) = let (p,(l,e)) = ys !! (findPosition (u+1,v+1)) in
                                 let (m,(i,o)) = ys !! (findPosition (u+1,v-1)) in
                                 let (a,(b,n)) = ys !! (findPosition (u-1,v+1)) in
                                 let (s,(f,g)) = ys !! (findPosition (u-1,v-1)) in
                                 let (dt,(ro,co)) = ys !! (findPosition (u+2,v+2)) in
                                 let (dy,(row,col)) = ys !! (findPosition (u+2,v-2)) in
                                 let (dx,(ry,cy)) = ys !! (findPosition (u-2,v+2)) in
                                 let (dq,(ru,cu)) = ys !! (findPosition (u-2,v-2)) in
                                  validMoveauxaux ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu)) where
{- validMoveauxaux ListwithPosition Move Move (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position)
    Returns True if the moves are valid for Player Red, returns false otherwise. First move is the position the piece moves from. Second move it the position the player wants to move to.
    All (String,Position) corresponds to various positions and strings around the first Move. The positions in order are, if we consider that first move are in the center,
    LowerRight, LowerLeft, UpperRight,Upperleft, LowerRightLowerRight, LowerLeftLowerLeft, UpperRightUpperRight, UpperLeftUpperLeft.   
    PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
    RETURNS: Bool   
-}
                                      validMoveauxaux:: ListwithPosition -> Move -> Move -> (String, Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String, Position) -> Bool
                                      validMoveauxaux ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
                                          |(u,v) == (w-2,q-2) = validMoveRedJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
                                          |(u,v) == (w-2,q+2) = validMoveRedJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu)) 
                                          |(u,v) == (w+2,q+2) = validMoveRedJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
                                          |(u,v) == (w+2,q-2) = validMoveRedJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
                                          |otherwise = validMoveAll ys (u,v) (w,q)
                                      
                                      
{- validMoveRedJump ListwithPosition Move Move (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position)
    Returns True if the moves are valid for Player Red, returns false otherwise. First move is the position the piece moves from. Second move it the position the player wants to move to.
    All (String,Position) corresponds to various positions and strings around the first Move. The positions in order are, if we consider that first move are in the center,
    LowerRight, LowerLeft, UpperRight,Upperleft, LowerRightLowerRight, LowerLeftLowerLeft, UpperRightUpperRight, UpperLeftUpperLeft.   
    PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
    RETURNS: Bool   
-}                                      
validMoveRedJump:: ListwithPosition -> Move -> Move -> (String, Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String, Position) -> Bool

validMoveRedJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))                                      
    |(u == 1 || u == 2) && (v == 1 || v == 2) = if ((p == "w" || p == "W") && dt == ".") then True else False
    |(u == 1 || u == 2) && (v == 7 || v == 8) = if ((m == "w" || m == "W") && dy == ".") then True else False
    |(u == 7 || u == 8) && (v == 1 || v == 2) = if ((a == "w" || a == "W") && dx == ".") then True else False
    |(u == 7 || u == 8) && (v == 7 || v == 8) = if ((s == "w" || s == "W") && dq == ".") then True else False
    |(u == 1 || u == 2) && (v >= 3 && v <= 6) = if ((p == "w" || p == "W") && dt == ".") || ((m == "w" || m == "W") && dy == ".") then True else False
    |(u >= 3 && u <= 6) && (v == 1 || v == 2) = if ((a == "w" || a == "W") && dx == ".") || ((p == "w" || p == "W") && dt == ".") then True else False
    |(u == 7 || u == 8) && (v >=3 && v <= 6) = if ((s == "w" || s == "W") && dq == ".") || ((a == "w" || a == "W") && dx == ".") then True else False
    |(u >= 3 && u <= 6) && (v == 7 || v == 8) = if ((s == "w" || s == "W") && dq == ".") || ((m == "w" || m == "W") && dy == ".") then True else False
    |otherwise = if ((p == "w" || p == "W") && dt == ".") || ((m == "w" || m == "W") && dy == ".") || ((a == "w" || a == "W") && dx == ".") || ((s == "w" || s == "W") && dq == ".") then True else False
  

{- validMoveauxWhite ListwithPosition Move Move
    Returns True if the moves are valid for Player White, returns false otherwise. First move is the position the piece moves from. Second move it the position
    the player wants to move to. 
    PRE: Valid move (row 1-8, column 1-8), no numbers outside the board.  
    RETURNS: Bool
    EXAMPLES: validMoveauxWhite [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))] (7,7) (8,8) = False
            validMoveauxWhite [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",   (2,8)),("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))] (6,6) (5,5) = True 
-}
validMoveauxWhite ::ListwithPosition -> Move -> Move -> Bool
validMoveauxWhite [] _ _ = False
validMoveauxWhite ys (u,v) (w,q) = let (p,(l,e)) = ys !! (findPosition (u+1,v+1)) in
                                   let (m,(i,o)) = ys !! (findPosition (u+1,v-1)) in
                                   let (a,(b,n)) = ys !! (findPosition (u-1,v+1)) in
                                   let (s,(f,g)) = ys !! (findPosition (u-1,v-1)) in
                                   let (dt,(ro,co)) = ys !! (findPosition (u+2,v+2)) in
                                   let (dy,(row,col)) = ys !! (findPosition (u+2,v-2)) in
                                   let (dx,(ry,cy)) = ys !! (findPosition (u-2,v+2)) in
                                   let (dq,(ru,cu)) = ys !! (findPosition (u-2,v-2)) in
                                       validMoveauxaux ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu)) where
{- validMoveauxaux ListwithPosition Move Move (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position)
    Checks first move with second move to see if you want to jump or take a step. If you want to jump it runs the function validMoveWhiteJump. Otherwise runs validMoveAll. 
    All (String,Position) corresponds to various positions and strings around the first Move. The positions in order are, if we consider that first move are in the center,
    LowerRight, LowerLeft, UpperRight,Upperleft, LowerRightLowerRight, LowerLeftLowerLeft, UpperRightUpperRight, UpperLeftUpperLeft.   
    PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
    RETURNS: Bool   
-}
                                       validMoveauxaux:: ListwithPosition -> Move -> Move -> (String, Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String, Position) -> Bool
                                       validMoveauxaux ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
                                           |(u,v) == (w-2,q-2) = validMoveWhiteJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
                                           |(u,v) == (w-2,q+2) = validMoveWhiteJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu)) 
                                           |(u,v) == (w+2,q+2) = validMoveWhiteJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
                                           |(u,v) == (w+2,q-2) = validMoveWhiteJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
                                           |otherwise = validMoveAll ys (u,v) (w,q)


{- validMoveAll ListwithPosition Move Move
    Returns true if you have a valid move were you move your piece one step diagonally from the starting position (first move). Works for both players.
    VARIANT: Length of list
    PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
    RETURNS: Bool
    EXAMPLES: validMoveAll [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),                 
              ("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))] (3,3) (4,4) = True 
              validMoveAll [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))] (3,3) (8,8) = False
-}
validMoveAll :: ListwithPosition -> Move -> Move -> Bool
validMoveAll [] _ _ = False                                                      
validMoveAll ((y,(c,d)):ys) (u,v) (w,q) 
    |(u,v) == (w-1,q-1) = if (c,d) == (w,q) && y == "." then True else validMoveAll ys (u,v) (w,q)
    |(u,v) == (w-1,q+1) = if (c,d) == (w,q) && y == "." then True else validMoveAll ys (u,v) (w,q)
    |(u,v) == (w+1,q+1) = if (c,d) == (w,q) && y == "." then True else validMoveAll ys (u,v) (w,q)
    |(u,v) == (w+1,q-1) = if (c,d) == (w,q) && y == "." then True else validMoveAll ys (u,v) (w,q)
    |otherwise = False
    
   
{- validMoveWhiteJump ListwithPosition Move Move (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position)
    Returns True if the moves are valid for Player White, returns false otherwise. First move is the position the piece moves from. Second move it the position the player wants to move to.
    This function considers a valid move a jump, that is to say two steps diagonally from the starting position (first move argument).
    All (String,Position) corresponds to various positions and strings around the first Move. The positions in order are, if we consider that first move are in the center,
    LowerRight, LowerLeft, UpperRight,Upperleft, LowerRightLowerRight, LowerLeftLowerLeft, UpperRightUpperRight, UpperLeftUpperLeft.   
    PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. Valid (String,Position) around the first move according to the order above.  
    RETURNS: Bool   
-}    
validMoveWhiteJump:: ListwithPosition -> Move -> Move -> (String, Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String, Position) -> Bool
validMoveWhiteJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))                                      
    |(u == 1 || u == 2) && (v == 1 || v == 2) = if ((p == "r" || p == "R") && dt == ".") then True else False
    |(u == 1 || u == 2) && (v == 7 || v == 8) = if ((m == "r" || m == "R") && dy == ".") then True else False
    |(u == 7 || u == 8) && (v == 1 || v == 2) = if ((a == "r" || a == "R") && dx == ".") then True else False
    |(u == 7 || u == 8) && (v == 7 || v == 8) = if ((s == "r" || s == "R") && dq == ".") then True else False
    |(u == 1 || u == 2) && (v >= 3 && v <= 6) = if ((p == "r" || p == "R") && dt == ".") || ((m == "r" || m == "R") && dy == ".") then True else False
    |(u >= 3 && u <= 6) && (v == 1 || v == 2) = if ((a == "r" || a == "R") && dx == ".") || ((p == "r" || p == "R") && dt == ".") then True else False
    |(u == 7 || u == 8) && (v >=3 && v <= 6) = if ((s == "r" || s == "R") && dq == ".") || ((a == "r" || a == "R") && dx == ".") then True else False
    |(u >= 3 && u <= 6) && (v == 7 || v == 8) = if ((s == "r" || s == "R") && dq == ".") || ((m == "r" || m == "R") && dy == ".") then True else False
    |otherwise = if ((p == "r" || p == "R") && dt == ".") || ((m == "r" || m == "R") && dy == ".") || ((a == "r" || a == "R") && dx == ".") || ((s == "r" || s == "R") && dq == ".") then True else False   
        
        
{- validchoiceRed GameState Move
    Returns true if you have a valid choice of player piece. That means that the player Red may only move red pieces. Move in this case is the position you want to move from.
    Note that this function only cares that you select one correct piece, it does not care if you can move from there or not. 
    VARIANT: Length of list
    PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
    RETURNS: Bool
    EXAMPLES: validchoiceRed [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],        
              [".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]] (3,3) = True
              validchoiceRed [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]] (4,4) = False
-}        
validchoiceRed :: GameState -> Move -> Bool
validchoiceRed (x:xs) (s,t) = validPlaceaux (concat (addPositionGameState (x:xs))) (s,t) where
{- validPlaceaux ListwithPosition Move
    Returns true if you have a valid choice of player piece. That means that the player Red may only move red pieces. Move in this case is the position you want to move from.
    It checks which position you want to move from and checks which piece is at that position. If the piece is "r" or "R" then True. Else False. 
    Note that this function only cares that you select one correct piece, it does not care if you can move from there or not. 
    VARIANT: Length of list
    PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
    RETURNS: Bool
    EXAMPLES: validPlaceaux [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))] (1,1) = True
-}
    validPlaceaux:: ListwithPosition -> Move -> Bool
    validPlaceaux [] _ = False
    validPlaceaux ((y,(c,d)):ys) (s,t)
        |c == s && d == t = if (y == "r" || y == "R") then True else False 
        |otherwise = validPlaceaux ys (s,t)
  

{- validchoiceWhite GameState Move
    Returns true if you have a valid choice of player piece. That means that the player White may only move white pieces. Move in this case is the position you want to move from.
    Note that this function only cares that you select one correct piece, it does not care if you can move from there or not. 
    VARIANT: Length of list
    PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
    RETURNS: Bool
    EXAMPLES: validchoiceWhite [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],        
              [".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]] (5,5) = False
              validchoiceWhite [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]] (8,8) = True
-}  
validchoiceWhite :: GameState -> Move -> Bool
validchoiceWhite (x:xs) (s,t) = validPlaceaux (concat (addPositionGameState (x:xs))) (s,t) where
{- validPlaceaux ListwithPosition Move
    Returns true if you have a valid choice of player piece. That means that the player White may only move white pieces. Move in this case is the position you want to move from.
    It checks which position you want to move from and checks which piece is at that position. If the piece is "w" or "W" then True. Else False. 
    Note that this function only cares that you select one correct piece, it does not care if you can move from there or not. 
    VARIANT: Length of list
    PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
    RETURNS: Bool
    EXAMPLES: validPlaceaux [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))] (7,7) = True
-}
    validPlaceaux:: ListwithPosition -> Move -> Bool
    validPlaceaux [] _ = False
    validPlaceaux ((y,(c,d)):ys) (s,t)
        |c == s && d == t = if (y == "w" || y == "W") then True else False 
        |otherwise = validPlaceaux ys (s,t)        

{- playMove GameState Move Move
    Returns a GameState in which the player has moved from first Move to second Move. 
    PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
    RETURNS: GameState
    EXAMPLES: playMove [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],          
                        [".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]] (3,3) (4,4) = 
                        [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".",".",".","r",".","r","."],[".",".",".","r",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]]
-}
playMove :: GameState -> Move -> Move -> GameState
playMove (x:xs) (fromrow,fromcol) (torow,tocol) = makeGamestate (removePosition (playMoveAux (concat (addPositionGameState (x:xs))) (fromrow,fromcol) (torow,tocol))) where
{- playMoveAux ListwithPosition Move Move
    Returns a list with positions in which the player has moved from first Move to second Move. 
    PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
    RETURNS: ListwithPosition
    EXAMPLES: playMoveAux [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),
                        ("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))] (3,3) (4,4) = 
                        [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),("r",(3,1)),(".",(3,2)),(".",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),("r",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))]
-}

    playMoveAux::ListwithPosition -> Move -> Move -> ListwithPosition
    playMoveAux [] _ _ = []
    playMoveAux all@(x:xs) (fromrow,fromcol) (torow,tocol) = let (r,(s,t)) = all !! (findPosition (fromrow,fromcol)) in
                                                             let (q,(w,e)) = all !! (findPosition (torow,tocol)) in
                                                             let (p,(l,ef)) = all !! (findPosition (fromrow+1,fromcol+1)) in
                                                             let (m,(i,o)) = all !! (findPosition (fromrow+1,fromcol-1)) in
                                                             let (a,(b,n)) = all !! (findPosition (fromrow-1,fromcol+1)) in
                                                             let (xa,(f,g)) = all !! (findPosition (fromrow-1,fromcol-1)) in
                                                                 makeaMove (x:xs) (r,(s,t)) (q,(w,e)) (fromrow,fromcol) (torow,tocol) (p,(l,ef)) (m,(i,o)) (a,(b,n)) (xa,(f,g)) where
{- makeaMove ListwithPosition (String,Position) (String,Position) Move Move (String,Position) (String,Position) (String,Position) (String,Position)
    Returns a list with positions. All (String,Position) corresponds to various positions and strings around the first Move, except the first one which is the piece corresponding to the first Move.
    The positions in order are, if we consider that first Move are in the center, LowerRight, LowerLeft, UpperRight,Upperleft. It deletes the (String,Position) you want to move to and inserts the piece
    at the same position. It then deletes the piece from the starting position and inserts a "." since it's now empty.  
    PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
    RETURNS: ListwithPosition
-}
                                                                     makeaMove all@(x:xs) (r,(s,t)) (q,(w,e)) (fromrow,fromcol) (torow,tocol) (p,(l,ef)) (m,(i,o)) (a,(b,n)) (xa,(f,g))
                                                                         |(fromrow,fromcol) == (torow-1,tocol-1) = insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))
                                                                         |(fromrow,fromcol) == (torow-1,tocol+1) = insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))
                                                                         |(fromrow,fromcol) == (torow+1,tocol+1) = insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))
                                                                         |(fromrow,fromcol) == (torow+1,tocol-1) = insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))
                                                                         |(fromrow,fromcol) == (torow-2,tocol-2) = insertAt (".",(fromrow +1,fromcol +1)) (delete (p,(fromrow +1,fromcol +1)) (insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))))
                                                                         |(fromrow,fromcol) == (torow-2,tocol+2) = insertAt (".",(fromrow +1,fromcol-1)) (delete (m,(fromrow +1,fromcol-1)) (insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))))
                                                                         |(fromrow,fromcol) == (torow+2,tocol+2) = insertAt (".",(fromrow -1,fromcol-1)) (delete (xa,(fromrow -1,fromcol-1)) (insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))))
                                                                         |(fromrow,fromcol) == (torow+2,tocol-2) = insertAt (".",(fromrow -1,fromcol+1)) (delete (a,(fromrow -1,fromcol+1)) (insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))))
                                                                                                                    


                        

{- insertAt:: (String,Position) ListwithPosition 
    Inserts a (String,Position) at the correct position in a list with the first element being position 0, second one being position 1 and so forth. 
    PRE: No positions bigger than (8,8), No positions smaller than (1,1)
    RETURNS: ListwithPosition
    EXAMPLES: insertAt ("r",(0,1)) [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(0,1)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8))] = 
                                    [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(0,1)),("r",(0,1)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8))]
              insertAt ("r",(1,2)) [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(0,1)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8))] = 
              [("r",(1,1)),("r",(1,2)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(0,1)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8))]
-}
insertAt:: (String,Position) -> ListwithPosition -> ListwithPosition
insertAt (r,(x,y)) [] = []
insertAt (r,(x,y)) ((a,(b,c)):xs) = let ((d,(f,(g,h))):ys) = zip [0..] ((a,(b,c)):xs) in
                                      let position = (findPosition (x,y)) in
                                          insertatAux (r,(x,y)) ((d,(f,(g,h))):ys) ((a,(b,c)):xs) position where
{- insertatAux:: (String,Position) [(Int, (String,Position))] -> ListwithPosition -> Int 
    Inserts a (String,Position) at the correct position in a list with the first element being position 0, second one being position 1 and so forth.
    It does this by comparing the two Ints provided in the arguments, if they are the same it inserts the (String,Position) at that place in the list. 
    PRE: No positions bigger than (8,8), No positions smaller than (1,1). The last Int provided must be the one you get from inserting (x,y) into findPosition.  
    RETURNS: ListwithPosition
    EXAMPLES: insertatAux ("r",(1,2)) [(0,("r",(1,1))),(1,(".",(1,2))),(2,("r",(1,3))),(3,(".",(1,4))),(4,("r",(1,5))),(5,(".",(1,6))),(6,("r",(1,7))),(7,(".",(1,8)))]
              [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8))] 1 = 
              [("r",(1,1)),("r",(1,2)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8))]
-}
                                              insertatAux :: (String,Position) -> [(Int, (String, Position))] -> ListwithPosition -> Int -> ListwithPosition
                                              insertatAux (r,(x,y)) [] [] _ = [] 
                                              insertatAux (r,(x,y)) ((d,(f,(g,h))):ys) ((a,(b,c)):xs) position
                                                  |position == d = (r,(x,y)):(a,(b,c)):xs
                                                  |x == 8 && y == 8 = (a,(b,c)):xs ++ [(r,(x,y))] 
                                                  |otherwise = [(f,(g,h))] ++ insertatAux (r,(x,y)) ys xs position 

                                                

{- findPosition Position
    Finds the number indexed at that position. First element is indexed at 0, second element is indexed at 1 and so forth. 
    Otherwise is an outlier, merely there to make the code complile and should under no circumstances be used. 
    PRE: No positions bigger than (8,8), No positions smaller than (1,1).  
    RETURNS: Int
    EXAMPLES: findPosition (1,1) = 0
              findPosition (7,8) = 55
-}

findPosition :: Position -> Int                                                             
findPosition (b,c)
    |b == 1 = 0 + c-1
    |b == 2 = 8 + c-1
    |b == 3 = 16 + c-1
    |b == 4 = 24 + c-1
    |b == 5 = 32 + c-1
    |b == 6 = 40 + c-1
    |b == 7 = 48 + c-1
    |b == 8 = 56 + c-1
    |otherwise = 4
    
    
victoryRed :: GameState -> Bool
victoryRed (x:xs) = victoryAux (concat (x:xs)) where
    victoryAux [] = True
    victoryAux (y:ys)
        |y == "w" || y == "W" = False
        |otherwise = victoryAux ys
    
victoryWhite :: GameState -> Bool
victoryWhite (x:xs) = victoryAux (concat (x:xs)) where
    victoryAux [] = True
    victoryAux (y:ys)
        |y == "r" || y == "R" = False
        |otherwise = victoryAux ys    

printMove :: String -> Move -> Move -> IO ()
printMove player (row1, column1) (row2, column2) = putStrLn $ player ++ " " ++ "moves from  " ++ "(" ++ (show row1) ++ " , " ++ (show column1) ++ ")" ++ "  to  " ++ "(" ++ (show row2) ++ " , " ++ (show column2) ++ ")"    


printboard :: Show a => [[a]] -> IO ()
printboard (x:y:z:q:a:b:c:d) = do
    putStrLn $ "   " ++ "1" ++ "   " ++ "2" ++ "   " ++ "3" ++ "   " ++ "4" ++ "   " ++ "5" ++ "   " ++ "6" ++ "   " ++ "7" ++ "   " ++ "8"
    putStrLn $ "1" ++ (show x)
    putStrLn $ "2" ++ (show y)
    putStrLn $ "3" ++ (show z)
    putStrLn $ "4" ++ (show q)
    putStrLn $ "5" ++ (show a)
    putStrLn $ "6" ++ (show b)
    putStrLn $ "7" ++ (show c)
    putStrLn $ "8" ++ (show (concat d))
    
quitPlease :: IO ()    
quitPlease = do
    putStrLn "do you want to quit? (yes/no)?"
    answer <- getLine 
    when (answer == "yes") main
    
checkPosition :: GameState -> Move -> Move -> Bool    
checkPosition (x:xs) (u,v) (w,q)
    |(u,v) == (w-2,q-2) = checkPositionsAux (concat (addPositionGameState (x:xs))) (w,q)
    |(u,v) == (w+2,q+2) = checkPositionsAux (concat (addPositionGameState (x:xs))) (w,q)
    |(u,v) == (w-2,q+2) = checkPositionsAux (concat (addPositionGameState (x:xs))) (w,q)
    |(u,v) == (w+2,q-2) = checkPositionsAux (concat (addPositionGameState (x:xs))) (w,q)
    |otherwise = False

checkPositionwhite :: GameState -> Move -> Move -> Bool    
checkPositionwhite (x:xs) (u,v) (w,q)
    |(u,v) == (w-2,q-2) = checkPositionsAuxwhite (concat (addPositionGameState (x:xs))) (w,q)
    |(u,v) == (w+2,q+2) = checkPositionsAuxwhite (concat (addPositionGameState (x:xs))) (w,q)
    |(u,v) == (w-2,q+2) = checkPositionsAuxwhite (concat (addPositionGameState (x:xs))) (w,q)
    |(u,v) == (w+2,q-2) = checkPositionsAuxwhite (concat (addPositionGameState (x:xs))) (w,q)
    |otherwise = False

checkPositionsAux ::ListwithPosition -> Move -> Bool
checkPositionsAux ys (w,q) =
    let (hea:tai) = ys in
    let (z,(x,j)) = ys !! (findPosition (w,q)) in
    let (p,(l,e)) = ys !! (findPosition (w+1,q+1)) in
    let (m,(i,o)) = ys !! (findPosition (w+1,q-1)) in
    let (a,(b,n)) = ys !! (findPosition (w-1,q+1)) in
    let (s,(f,g)) = ys !! (findPosition (w-1,q-1)) in
    let (dt,(ro,co)) = ys !! (findPosition (w+2,q+2)) in
    let (dy,(row,col)) = ys !! (findPosition (w+2,q-2)) in
    let (dx,(ry,cy)) = ys !! (findPosition (w-2,q+2)) in
    let (dq,(ru,cu)) = ys !! (findPosition (w-2,q-2)) in
    check (hea:tai) (z,(x,j)) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu)) where
        check (hea:tai) (z,(x,j)) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
            |(x == 1 || x == 2) && (j == 1 || j == 2) = if ((p == "w" || p == "W") && dt == ".") then True else False
            |(x == 1 || x == 2) && (j == 7 || j == 8) = if ((m == "w" || m == "W") && dy == ".") then True else False
            |(x == 7 || x == 8) && (j == 1 || j == 2) = if ((a == "w" || a == "W") && dx == ".") then True else False
            |(x == 7 || x == 8) && (j == 7 || j == 8) = if ((s == "w" || s == "W") && dq == ".") then True else False
            |(x == 1 || x == 2) && (j >= 3 && j <= 6) = if ((p == "w" || p == "W") && dt == ".") || ((m == "w" || m == "W") && dy == ".") then True else False
            |(x >= 3 && x <= 6) && (j == 1 || j == 2) = if ((a == "w" || a == "W") && dx == ".") || ((p == "w" || p == "W") && dt == ".") then True else False
            |(x == 7 || x == 8) && (j >=3 && j <= 6) = if ((s == "w" || s == "W") && dq == ".") || ((a == "w" || a == "W") && dx == ".") then True else False
            |(x >= 3 && x <= 6) && (j == 7 || j == 8) = if ((s == "w" || s == "W") && dq == ".") || ((m == "w" || m == "W") && dy == ".") then True else False
            |otherwise = if ((p == "w" || p == "W") && dt == ".") || ((m == "w" || m == "W") && dy == ".") || ((a == "w" || a == "W") && dx == ".") || ((s == "w" || s == "W") && dq == ".") then True else False

checkPositionsAuxwhite ::ListwithPosition -> Move -> Bool            
checkPositionsAuxwhite ys (w,q) =
    let (hea:tai) = ys in
    let (z,(x,j)) = ys !! (findPosition (w,q)) in
    let (p,(l,e)) = ys !! (findPosition (w+1,q+1)) in
    let (m,(i,o)) = ys !! (findPosition (w+1,q-1)) in
    let (a,(b,n)) = ys !! (findPosition (w-1,q+1)) in
    let (s,(f,g)) = ys !! (findPosition (w-1,q-1)) in
    let (dt,(ro,co)) = ys !! (findPosition (w+2,q+2)) in
    let (dy,(row,col)) = ys !! (findPosition (w+2,q-2)) in
    let (dx,(ry,cy)) = ys !! (findPosition (w-2,q+2)) in
    let (dq,(ru,cu)) = ys !! (findPosition (w-2,q-2)) in
    check (hea:tai) (z,(x,j)) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu)) where
        check (hea:tai) (z,(x,j)) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
            |(x == 1 || x == 2) && (j == 1 || j == 2) = if ((p == "r" || p == "R") && dt == ".") then True else False
            |(x == 1 || x == 2) && (j == 7 || j == 8) = if ((m == "r" || m == "R") && dy == ".") then True else False
            |(x == 7 || x == 8) && (j == 1 || j == 2) = if ((a == "r" || a == "R") && dx == ".") then True else False
            |(x == 7 || x == 8) && (j == 7 || j == 8) = if ((s == "r" || s == "R") && dq == ".") then True else False
            |(x == 1 || x == 2) && (j >= 3 && j <= 6) = if ((p == "r" || p == "R")  && dt == ".") || ((m == "r" || m == "R") && dy == ".") then True else False
            |(x >= 3 && x <= 6) && (j == 1 || j == 2) = if ((a == "r" || a == "R") && dx == ".") || ((p == "r" || p == "R") && dt == ".") then True else False
            |(x == 7 || x == 8) && (j >=3 && j <= 6) = if ((s == "r" || s == "r") && dq == ".") || ((a == "r" || a == "R") && dx == ".") then True else False
            |(x >= 3 && x <= 6) && (j == 7 || j == 8) = if ((s == "r" || s == "R") && dq == ".") || ((m == "r" || m == "R") && dy == ".") then True else False
            |otherwise = if ((p == "r" || p == "R") && dt == ".") || ((m == "r" || m == "R") && dy == ".") || ((a == "r" || a == "R") && dx == ".") || ((s == "r" || s == "R") && dq == ".") then True else False
            
