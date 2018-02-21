import Data.Char
import Control.Monad
import Control.Exception
import Debug.Trace
import Data.List

type GameState = [[String]]
type GameStateWithPosition = [[(String,Position)]]
data Player = Player Color Type

data Color = Red | White | None
data Type = Normal | Queen | NotHere

type ListwithPosition = [(String,Position)]
type List = [[Char]] 
type Player1 = String
type Position = (Int, Int)
type Move = (Int, Int)

readPiece:: Player -> String
readPiece (Player Red Normal) = "r"
readPiece (Player Red Queen) = "R"
readPiece (Player White Normal) = "w"
readPiece (Player White Queen) = "W"
readPiece (Player None NotHere) = "."

showPiece "w" = readPiece (Player White Normal)
showPiece "W" = readPiece (Player White Queen)
showPiece "r" = readPiece (Player Red Normal)
showPiece "R" = readPiece (Player Red Queen)
showPiece "." = readPiece (Player None NotHere)

exchangePiece x = showPiece x

upgradePiece "r" = exchangePiece "R"
upgradePiece "w" = exchangePiece "W"
upgradePiece "W" = exchangePiece "W"
upgradePiece "R" = exchangePiece "R"

sizeofBoardish::List
sizeofBoardish = replicate 64 "."
  
addPosition::ListwithPosition
addPosition = let (x:xs) = insertPlayer in
                  addPositionAux (x:xs) (1,1) where
                      addPositionAux [] _ = []
                      addPositionAux all@(x:xs) (a,b)
                          |b < 8 = [(x,(a,b))] ++ addPositionAux xs (a,b+1)
                          |b == 8 = [(x,(a,b))] ++ addPositionAux xs (a+1,b-7)

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
insertPlayer = let (x:y:xs) = sizeofBoardish in
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
  if (validPlaceRed gameState move1 && validMoveRed gameState move1 move2) then do
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
  if (validPlaceWhite gameState move1 && validMoveWhite gameState move1 move2) then do
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
    if (validPlaceWhite newGameState move3 && validMoveWhite newGameState move3 move4) then do
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
    if (validPlaceRed newGameState move3 && validMoveRed newGameState move3 move4) then do
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
        
        
        
validPlaceRed :: GameState -> Move -> Bool
validPlaceRed (x:xs) (s,t) = validPlaceaux (concat (addPositionGameState (x:xs))) (s,t) where
    validPlaceaux [] _ = False
    validPlaceaux ((y,(c,d)):ys) (s,t)
        |c == s && d == t = if (y == "r" || y == "R") then True else False 
        |otherwise = validPlaceaux ys (s,t)
        
validPlaceWhite :: GameState -> Move -> Bool
validPlaceWhite (x:xs) (s,t) = validPlaceaux (concat (addPositionGameState (x:xs))) (s,t) where
    validPlaceaux [] _ = False
    validPlaceaux ((y,(c,d)):ys) (s,t)
        |c == s && d == t = if (y == "w" || y == "W") then True else False 
        |otherwise = validPlaceaux ys (s,t)        

playMove :: GameState -> Move -> Move -> GameState
playMove (x:xs) (fromrow,fromcol) (torow,tocol) = makeGamestate (removePosition (playMoveAux (concat (addPositionGameState (x:xs))) (fromrow,fromcol) (torow,tocol))) where
    playMoveAux [] _ _ = []
    playMoveAux all@(x:xs) (fromrow,fromcol) (torow,tocol) = let (r,(s,t)) = all !! (findPosition (fromrow,fromcol)) in -- OBS DOES NOT WORK NEED TO MAKE OWN INSERT FUNCTION
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

printMove :: Player1 -> Move -> Move -> IO ()
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
            
