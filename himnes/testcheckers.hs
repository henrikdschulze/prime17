import Data.Char
import Control.Monad
import Control.Exception
import System.Random
import Debug.Trace
import Data.List

type GameState = [[String]]
type GameStateWithPosition = [[(String,Position)]]
data Player = Player Color Type

data Color = Red | White
data Type = Normal | Queen

type BoardwithPosition = [(String,Position)]
type Board = [[Char]] 
type Player1 = String
type Player2 = String
type Position = (Int, Int)
type Move = (Int, Int)

readPiece:: Player -> String
readPiece (Player Red Normal) = "r"
readPiece (Player Red Queen) = "R"
readPiece (Player White Normal) = "w"
readPiece (Player White Queen) = "W"

sizeofBoardish::Board
sizeofBoardish = replicate 64 "."
  
addPosition::BoardwithPosition
addPosition = let (x:xs) = insertPlayer in
                  addPositionAux (x:xs) (1,1) where
                      addPositionAux [] _ = []
                      addPositionAux all@(x:xs) (a,b)
                          |b < 8 = [(x,(a,b))] ++ addPositionAux xs (a,b+1)
                          |b == 8 = [(x,(a,b))] ++ addPositionAux xs (a+1,b-7)

removePosition :: BoardwithPosition -> Board
removePosition [] = []
removePosition ((x,(a,b)):xs) = [x] ++ removePosition xs

addPositionGameState :: GameState -> GameStateWithPosition
addPositionGameState (x:xs) = letmakeRowswithPieces (addPositionAux (concat(x:xs)) (1,1)) where
    addPositionAux [] _ = []
    addPositionAux all@(x:xs) (a,b)
        |b < 8 = [(x,(a,b))] ++ addPositionAux xs (a,b+1)
        |b == 8 = [(x,(a,b))] ++ addPositionAux xs (a+1,b-7)


insertPlayer::Board
insertPlayer = let (x:y:xs) = sizeofBoardish in
                   insertPlayerRed (x:y:xs) readPiece (Player Red Normal) (1) where
                       insertPlayerRed [] _ _ _ = []
                       insertPlayerRed (x:y:xs) readPiece (Player Red Normal) (num)
                           |num <= 4 = (readPiece (Player Red Normal)):y:(insertPlayerRed xs readPiece (Player Red Normal) (num+1))
                           |num > 4 && num <= 8 = y:(readPiece (Player Red Normal)):(insertPlayerRed xs readPiece (Player Red Normal) (num+1))
                           |num > 8 && num < 12 = (readPiece (Player Red Normal)):y:(insertPlayerRed xs readPiece (Player Red Normal) (num+1))
                           |num == 12 = (readPiece (Player Red Normal)):y:(reverse(insertPlayerWhite (reverse xs)))

insertPlayerWhite:: Board -> Board
insertPlayerWhite xs = insertPlayerWhiteAux xs readPiece (Player White Normal) (1) where
    insertPlayerWhiteAux [] _ _ _ = []
    insertPlayerWhiteAux (x:y:ys) readPiece (Player White Normal) (num)
        |num <= 4 = (readPiece (Player White Normal)):y:(insertPlayerWhiteAux ys readPiece (Player White Normal) (num+1))
        |num > 4 && num <= 8 = y:(readPiece (Player White Normal)):(insertPlayerWhiteAux ys readPiece (Player White Normal) (num+1))
        |num > 8 && num < 12 = (readPiece (Player White Normal)):y:(insertPlayerWhiteAux ys readPiece (Player White Normal) (num+1))
        |num == 12 = (readPiece (Player White Normal)):y:ys      
        
--letmakeRowswithPieces:: Board -> GameState        
letmakeRowswithPieces (x:xs) = rows (x:xs) where
    rows l@(x:xs) = [take 8 l] ++ rows (drop 8 l)
    rows [] = [] 

genGameState = do
    return (letmakeRowswithPieces (insertPlayer))    

main :: IO ()
main = do 
  putStrLn "Welcome to Checkers."
  gameState <- genGameState 
  play gameState                      

play gameState = do
    printboard gameState --works till here
    newGameState <- playerMove gameState --working on this part
    if victory newGameState then do
        putStrLn "Player won!"
        putStrLn ""
        quitPlease
        else do
            newNewGameState <- computerMove newGameState
            if victory newNewGameState then do
                putStrLn "Computer won!"
                putStrLn ""
                quitPlease
                                       else
                                       play newNewGameState 
      
playerMove gameState = do
  putStrLn "Player Red move from"
  move1 <- readMove
  putStrLn "Player Red move to"
  move2 <- readMove
  printMove "Player Red" move1 move2
  if (validPlaceRed gameState move1 && validMove gameState move1 move2) then 
    return $ playMove gameState move1 --working on this part
   else do
       putStrLn "Invalid Move. You can only move your own pieces and move diagonally"
       playerMove gameState

victory gameState = undefined       

readMove :: (IO Move) -- reads input from
readMove = do
  catch (do
    line <- getLine
    evaluate (read line))  -- evaluate required to force conversion of line to Move
    ((\_ -> do   -- exception handler
    putStrLn "Invalid input. Correct format: (row,column)" 
    readMove) :: SomeException -> IO Move)


validMove :: GameState -> Move -> Move -> Bool
validMove (x:xs) (u,v) (w,q) = validMoveaux (concat (addPositionGameState (x:xs))) (u,v) (w,q) where
    validMoveaux [] _ _ = False
    validMoveaux ((y,(c,d)):ys) (u,v) (w,q)
        |(u,v) == (w-1,q-1) = if (c,d) == (w,q) && y == "." then True else validMoveaux ys (u,v) (w,q)
        |(u,v) == (w-1,q+1) = if (c,d) == (w,q) && y == "." then True else validMoveaux ys (u,v) (w,q)
        |(u,v) == (w+1,q+1) = if (c,d) == (w,q) && y == "." then True else validMoveaux ys (u,v) (w,q)
        |(u,v) == (w+1,q-1) = if (c,d) == (w,q) && y == "." then True else validMoveaux ys (u,v) (w,q)
        |(u,v) == (w-2,q-2) = if (c,d) == (w,q) && y == "w" then True else validMoveaux ys (u,v) (w,q)
        |(u,v) == (w-2,q+2) = if (c,d) == (w,q) && y == "w" then True else validMoveaux ys (u,v) (w,q)
        |(u,v) == (w+2,q+2) = if (c,d) == (w,q) && y == "w" then True else validMoveaux ys (u,v) (w,q)
        |(u,v) == (w+2,q-2) = if (c,d) == (w,q) && y == "w" then True else validMoveaux ys (u,v) (w,q)
        |otherwise = False  
        
        
        
validPlaceRed :: GameState -> Move -> Bool
validPlaceRed (x:xs) (s,t) = validPlaceaux (concat (addPositionGameState (x:xs))) (s,t) where
    validPlaceaux [] _ = False
    validPlaceaux ((y,(c,d)):ys) (s,t)
        |c == s && d == t = if y == "r" then True else False 
        |otherwise = validPlaceaux ys (s,t)

playMove x y = undefined


-- validMove :: GameState -> Move -> Bool
-- validMove (a, _, _) (1, n) = a >= n && 3 >= n && n > 0
-- validMove (_, b, _) (2, n) = b >= n && 3 >= n && n > 0 
-- validMove (_, _, c) (3, n) = c >= n && 3 >= n && n > 0
-- validMove _ _ = False

printMove :: Player1 -> Move -> Move -> IO ()
printMove player (row1, column1) (row2, column2) = putStrLn $ player ++ " " ++ "moves from  " ++ "(" ++ (show row1) ++ " , " ++ (show column1) ++ ")" ++ "  to  " ++ "(" ++ (show row2) ++ " , " ++ (show column2) ++ ")"    
   
computerMove x = undefined


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
    
    
quitPlease = do
    putStrLn "do you want to play again? (yes/no)?"
    answer <- getLine 
    when (answer == "yes") main
