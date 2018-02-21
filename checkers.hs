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
              addPositionAux 
    [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))]
-}
                      addPositionAux:: List -> Position -> ListwithPosition
                      addPositionAux [] _ = []
                      addPositionAux all@(x:xs) (a,b)
                          |b < 8 = [(x,(a,b))] ++ addPositionAux xs (a,b+1)
                          |b == 8 = [(x,(a,b))] ++ addPositionAux xs (a+1,b-7)

addpos (x:xs) (a,b)                          
                          
removePosition :: ListwithPosition -> List
removePosition [] = []
removePosition ((x,(a,b)):xs) = [x] ++ removePosition xs

addPositionGameState :: GameState -> GameStateWithPosition
addPositionGameState (x:xs) = makeGamestate (addPositionAux (concat(x:xs)) (1,1)) where
    addPositionAux [] _ = []
    addPositionAux all@(x:xs) (a,b)
        |b < 8 = [(x,(a,b))] ++ addPositionAux xs (a,b+1)
        |b == 8 = [(x,(a,b))] ++ addPositionAux xs (a+1,b-7)


insertPlayer::List
insertPlayer = let (x:y:xs) = emptyBoard in
                   insertPlayerRed (x:y:xs) readPiece (Player Red Normal) (1) where
                       insertPlayerRed [] _ _ _ = []
                       insertPlayerRed (x:y:xs) readPiece (Player Red Normal) (num)
                           |num <= 4 = (readPiece (Player Red Normal)):y:(insertPlayerRed xs readPiece (Player Red Normal) (num+1))
                           |num > 4 && num <= 8 = y:(readPiece (Player Red Normal)):(insertPlayerRed xs readPiece (Player Red Normal) (num+1))
                           |num > 8 && num < 12 = (readPiece (Player Red Normal)):y:(insertPlayerRed xs readPiece (Player Red Normal) (num+1))
                           |num == 12 = (readPiece (Player Red Normal)):y:(reverse(insertPlayerWhite (reverse xs)))

insertPlayerWhite:: List -> List
insertPlayerWhite xs = insertPlayerWhiteAux xs readPiece (Player White Normal) (1) where
    insertPlayerWhiteAux [] _ _ _ = []
    insertPlayerWhiteAux (x:y:ys) readPiece (Player White Normal) (num)
        |num <= 4 = (readPiece (Player White Normal)):y:(insertPlayerWhiteAux ys readPiece (Player White Normal) (num+1))
        |num > 4 && num <= 8 = y:(readPiece (Player White Normal)):(insertPlayerWhiteAux ys readPiece (Player White Normal) (num+1))
        |num > 8 && num < 12 = (readPiece (Player White Normal)):y:(insertPlayerWhiteAux ys readPiece (Player White Normal) (num+1))
        |num == 12 = (readPiece (Player White Normal)):y:ys      

makeGamestate:: [a] -> [[a]]
makeGamestate (x:xs) = rows (x:xs) where
    rows l@(x:xs) = [take 8 l] ++ rows (drop 8 l)
    rows [] = [] 

genGameState:: IO GameState
genGameState = do
    return (makeGamestate (insertPlayer))    

main :: IO ()
main = do 
  putStrLn "Welcome to Checkers."
  gameState <- genGameState
  play gameState                      

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
      

       
      
checkifcanUpgradeRed :: Position -> Bool       
checkifcanUpgradeRed (a,b)
    |a == 8 = True
    |otherwise = False
    
checkifcanUpgradeWhite :: Position -> Bool
checkifcanUpgradeWhite (a,b)
    |a == 1 = True
    |otherwise = False
       
upgradeRed :: GameState -> Position -> GameState
upgradeRed newGameState (a,b) = makeGamestate (removePosition (checkifUpgradeAux (concat (addPositionGameState newGameState)) (a,b))) where
    checkifUpgradeAux ((y,(c,d)):ys) (a,b) = let (r,(s,t)) = ((y,(c,d)):ys) !! (findPosition (a,b)) in
                                                 checkUpgrade ((y,(c,d)):ys) (r,(s,t)) where
                                                     checkUpgrade all@((y,(c,d)):ys) (r,(s,t))
                                                         |s == 8 = insertAt ((upgradePiece r),(s,t)) (delete (r,(s,t)) all)
                                                         |otherwise = ((y,(c,d)):ys)
    
upgradeWhite:: GameState -> Position -> GameState
upgradeWhite newGameState (a,b) = makeGamestate (removePosition (checkifUpgradeAux (concat (addPositionGameState newGameState)) (a,b))) where
    checkifUpgradeAux ((y,(c,d)):ys) (a,b) = let (r,(s,t)) = ((y,(c,d)):ys) !! (findPosition (a,b)) in
                                                 checkUpgrade ((y,(c,d)):ys) (r,(s,t)) where
                                                     checkUpgrade all@((y,(c,d)):ys) (r,(s,t))
                                                         |s == 1 = insertAt ((upgradePiece r),(s,t)) (delete (r,(s,t)) all)
                                                         |otherwise = ((y,(c,d)):ys)       


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
         
         
readMove :: (IO Move) -- reads input from
readMove = do
  catch (do
    line <- getLine
    evaluate (read line))  -- evaluate required to force conversion of line to Move
    ((\_ -> do   -- exception handler
    putStrLn "Invalid input. Correct format: (row,column)" 
    readMove) :: SomeException -> IO Move)
    
    
    
validMoveRed :: GameState -> Move -> Move -> Bool
validMoveRed (x:xs) (u,v) (w,q) = validMoveauxRed (concat (addPositionGameState (x:xs))) (u,v) (w,q)

validMoveWhite :: GameState -> Move -> Move -> Bool
validMoveWhite (x:xs) (u,v) (w,q) = validMoveauxWhite (concat (addPositionGameState (x:xs))) (u,v) (w,q)

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
                                      validMoveauxaux ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
                                          |(u,v) == (w-2,q-2) = validMoveRedJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
                                          |(u,v) == (w-2,q+2) = validMoveRedJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu)) 
                                          |(u,v) == (w+2,q+2) = validMoveRedJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
                                          |(u,v) == (w+2,q-2) = validMoveRedJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
                                          |otherwise = validMoveAll ys (u,v) (w,q)
                                      
                                      
                                      
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
                                       validMoveauxaux ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
                                           |(u,v) == (w-2,q-2) = validMoveWhiteJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
                                           |(u,v) == (w-2,q+2) = validMoveWhiteJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu)) 
                                           |(u,v) == (w+2,q+2) = validMoveWhiteJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
                                           |(u,v) == (w+2,q-2) = validMoveWhiteJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
                                           |otherwise = validMoveAll ys (u,v) (w,q)

validMoveAll :: ListwithPosition -> Move -> Move -> Bool
validMoveAll [] _ _ = False                                                      
validMoveAll ((y,(c,d)):ys) (u,v) (w,q) 
    |(u,v) == (w-1,q-1) = if (c,d) == (w,q) && y == "." then True else validMoveAll ys (u,v) (w,q)
    |(u,v) == (w-1,q+1) = if (c,d) == (w,q) && y == "." then True else validMoveAll ys (u,v) (w,q)
    |(u,v) == (w+1,q+1) = if (c,d) == (w,q) && y == "." then True else validMoveAll ys (u,v) (w,q)
    |(u,v) == (w+1,q-1) = if (c,d) == (w,q) && y == "." then True else validMoveAll ys (u,v) (w,q)
    |otherwise = False
    
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
        
        
        
validchoiceRed :: GameState -> Move -> Bool
validchoiceRed (x:xs) (s,t) = validPlaceaux (concat (addPositionGameState (x:xs))) (s,t) where
    validPlaceaux [] _ = False
    validPlaceaux ((y,(c,d)):ys) (s,t)
        |c == s && d == t = if (y == "r" || y == "R") then True else False 
        |otherwise = validPlaceaux ys (s,t)
        
validchoiceWhite :: GameState -> Move -> Bool
validchoiceWhite (x:xs) (s,t) = validPlaceaux (concat (addPositionGameState (x:xs))) (s,t) where
    validPlaceaux [] _ = False
    validPlaceaux ((y,(c,d)):ys) (s,t)
        |c == s && d == t = if (y == "w" || y == "W") then True else False 
        |otherwise = validPlaceaux ys (s,t)        

playMove :: GameState -> Move -> Move -> GameState
playMove (x:xs) (fromrow,fromcol) (torow,tocol) = makeGamestate (removePosition (playMoveAux (concat (addPositionGameState (x:xs))) (fromrow,fromcol) (torow,tocol))) where
    playMoveAux [] _ _ = []
    playMoveAux all@(x:xs) (fromrow,fromcol) (torow,tocol) = let (r,(s,t)) = all !! (findPosition (fromrow,fromcol)) in
                                                             let (q,(w,e)) = all !! (findPosition (torow,tocol)) in
                                                             let (p,(l,ef)) = all !! (findPosition (fromrow+1,fromcol+1)) in
                                                             let (m,(i,o)) = all !! (findPosition (fromrow+1,fromcol-1)) in
                                                             let (a,(b,n)) = all !! (findPosition (fromrow-1,fromcol+1)) in
                                                             let (xa,(f,g)) = all !! (findPosition (fromrow-1,fromcol-1)) in
                                                                 makeaMove (x:xs) (r,(s,t)) (q,(w,e)) (fromrow,fromcol) (torow,tocol) (p,(l,ef)) (m,(i,o)) (a,(b,n)) (xa,(f,g)) where
                                                                     makeaMove all@(x:xs) (r,(s,t)) (q,(w,e)) (fromrow,fromcol) (torow,tocol) (p,(l,ef)) (m,(i,o)) (a,(b,n)) (xa,(f,g))
                                                                         |(fromrow,fromcol) == (torow-1,tocol-1) = insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))
                                                                         |(fromrow,fromcol) == (torow-1,tocol+1) = insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))
                                                                         |(fromrow,fromcol) == (torow+1,tocol+1) = insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))
                                                                         |(fromrow,fromcol) == (torow+1,tocol-1) = insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))
                                                                         |(fromrow,fromcol) == (torow-2,tocol-2) = insertAt (".",(fromrow +1,fromcol +1)) (delete (showPiece p,(fromrow +1,fromcol +1)) (insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))))
                                                                         |(fromrow,fromcol) == (torow-2,tocol+2) = insertAt (".",(fromrow +1,fromcol-1)) (delete (showPiece m,(fromrow +1,fromcol-1)) (insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))))
                                                                         |(fromrow,fromcol) == (torow+2,tocol+2) = insertAt (".",(fromrow -1,fromcol-1)) (delete (showPiece xa,(fromrow -1,fromcol-1)) (insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))))
                                                                         |(fromrow,fromcol) == (torow+2,tocol-2) = insertAt (".",(fromrow -1,fromcol+1)) (delete (showPiece a,(fromrow -1,fromcol+1)) (insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))))
                                                                                                                    


                        

insertAt:: (String,Position) -> ListwithPosition -> ListwithPosition
insertAt (r,(x,y)) [] = []
insertAt (r,(x,y)) ((a,(b,c)):xs) = let ((d,(f,(g,h))):ys) = zip [0..] ((a,(b,c)):xs) in
                                      let position = (findPosition (x,y)) in
                                          insertatAux (r,(x,y)) ((d,(f,(g,h))):ys) ((a,(b,c)):xs) position where
                                              insertatAux (r,(x,y)) [] [] _ = [] 
                                              insertatAux (r,(x,y)) ((d,(f,(g,h))):ys) ((a,(b,c)):xs) position
                                                         |position == d = (r,(x,y)):(a,(b,c)):xs
                                                         |x == 8 && y == 8 = (a,(b,c)):xs ++ [(r,(x,y))] 
                                                         |otherwise = [(f,(g,h))] ++ insertatAux (r,(x,y)) ys xs position 
           
findPosition :: (Eq a1, Num a1, Num a2) => (a1, a2) -> a2                                                             
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
            
