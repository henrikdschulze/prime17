--MADDE
import Data.Char
import Control.Monad
import Control.Exception
import System.Random
import Debug.Trace

--B = Black, W = White, P = Player
data Square = B | W | P Color Type deriving (Show,Eq)

-- N = Normal, Q = Queen
data Type = N | Q deriving (Show,Eq)
-- R = Red, Wh = White. 
data Color = R | Wh deriving (Show,Eq)

type Board = [Square]

type Position = (Int, Int)

type Move = (Int,Int)

{- sizeofBoard
    Makes a board with only black tiles. 
   RETURNS: Board
   EXAMPLES: sizeofBoard = [B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B]
-}  
sizeofBoard :: Board
sizeofBoard = replicate 64 B 


{- makecheckeredBoard
    Makes a board with black and white tiles
   RETURNS: Board
   EXAMPLES: makecheckeredBoard = [B,W,B,W,B,W,B,W,W,B,W,B,W,B,W,B,B,W,B,W,B,W,B,W,W,B,W,B,W,B,W,B,B,W,B,W,B,W,B,W,W,B,W,B,W,B,W,B,B,W,B,W,B,W,B,W,W,B,W,B,W,B,W,B]
-}  
makecheckeredBoard::Board
makecheckeredBoard = let (x:xs) = sizeofBoard in
                   makecheckeredBoardAux (x:xs) (1) where
{- makecheckeredBoardAux Board
    Takes a black board, returns a checkers board
    PRE: Needs a Board
   RETURNS: Board
   EXAMPLES: makecheckeredBoardAux sizeofBoard = [B,W,B,W,B,W,B,W,W,B,W,B,W,B,W,B,B,W,B,W,B,W,B,W,W,B,W,B,W,B,W,B,B,W,B,W,B,W,B,W,W,B,W,B,W,B,W,B,B,W,B,W,B,W,B,W,W,B,W,B,W,B,W,B]
-}  
                       --makecheckeredBoardAux:: Board -> Int -> Board
                       makecheckeredBoardAux [] _ = []
                       makecheckeredBoardAux (x:y:xs) (num) 
                           |num <= 4 = B:W:makecheckeredBoardAux xs (num +1)
                           |num > 4 && num <= 8 = W:B:makecheckeredBoardAux xs (num +1)
                           |otherwise = makecheckeredBoardAux (x:y:xs) (num -8)
                       

{- letmakeRows
    Chops the list of squares into 8 element rows.
   RETURNS: [[(Square,Position)]]
   EXAMPLES: letmakeRows = [[(B,(1,1)),(W,(1,2)),(B,(1,3)),(W,(1,4)),(B,(1,5)),(W,(1,6)),(B,(1,7)),(W,(1,8))],[(W,(2,1)),(B,(2,2)),(W,(2,3)),(B,(2,4)),(W,(2,5)),(B,(2,6)),(W,(2,7)),(B,(2,8))],[(B,(3,1)),(W,(3,2)),(B,(3,3)),(W,(3,4)),(B,(3,5)),(W,(3,6)),(B,(3,7)),(W,(3,8))],[(W,(4,1)),(B,(4,2)),(W,(4,3)),(B,(4,4)),(W,(4,5)),(B,(4,6)),(W,(4,7)),(B,(4,8))],[(B,(5,1)),(W,(5,2)),(B,(5,3)),(W,(5,4)),(B,(5,5)),(W,(5,6)),(B,(5,7)),(W,(5,8))],[(W,(6,1)),(B,(6,2)),(W,(6,3)),(B,(6,4)),(W,(6,5)),(B,(6,6)),(W,(6,7)),(B,(6,8))],[(B,(7,1)),(W,(7,2)),(B,(7,3)),(W,(7,4)),(B,(7,5)),(W,(7,6)),(B,(7,7)),(W,(7,8))],[(W,(8,1)),(B,(8,2)),(W,(8,3)),(B,(8,4)),(W,(8,5)),(B,(8,6)),(W,(8,7)),(B,(8,8))]]
-}  
letmakeRows :: [[(Square, Position)]]                       
letmakeRows = let (x:xs) = addPosition in
                  rows (x:xs) where
{- rows [(Square, Position)]
    Chops the list of squares into 8 element rows.
   RETURNS: [[(Square,Position)]]
   EXAMPLES: rows addPosition = [[(B,(1,1)),(W,(1,2)),(B,(1,3)),(W,(1,4)),(B,(1,5)),(W,(1,6)),(B,(1,7)),(W,(1,8))],[(W,(2,1)),(B,(2,2)),(W,(2,3)),(B,(2,4)),(W,(2,5)),(B,(2,6)),(W,(2,7)),(B,(2,8))],[(B,(3,1)),(W,(3,2)),(B,(3,3)),(W,(3,4)),(B,(3,5)),(W,(3,6)),(B,(3,7)),(W,(3,8))],[(W,(4,1)),(B,(4,2)),(W,(4,3)),(B,(4,4)),(W,(4,5)),(B,(4,6)),(W,(4,7)),(B,(4,8))],[(B,(5,1)),(W,(5,2)),(B,(5,3)),(W,(5,4)),(B,(5,5)),(W,(5,6)),(B,(5,7)),(W,(5,8))],[(W,(6,1)),(B,(6,2)),(W,(6,3)),(B,(6,4)),(W,(6,5)),(B,(6,6)),(W,(6,7)),(B,(6,8))],[(B,(7,1)),(W,(7,2)),(B,(7,3)),(W,(7,4)),(B,(7,5)),(W,(7,6)),(B,(7,7)),(W,(7,8))],[(W,(8,1)),(B,(8,2)),(W,(8,3)),(B,(8,4)),(W,(8,5)),(B,(8,6)),(W,(8,7)),(B,(8,8))]]
-}
                      rows:: [(Square, Position)] -> [[(Square, Position)]]
                      rows l@(x:xs) = [take 8 l] ++ rows (drop 8 l)
                      rows [] = []
                      


{- makeSquare
    makes the list of tiles into a readable square 8 x 8. 
   RETURNS: IO()
   EXAMPLES: makeSquare =   [(B,(1,1)),(W,(1,2)),(B,(1,3)),(W,(1,4)),(B,(1,5)),(W,(1,6)),(B,(1,7)),(W,(1,8))]
                            [(W,(2,1)),(B,(2,2)),(W,(2,3)),(B,(2,4)),(W,(2,5)),(B,(2,6)),(W,(2,7)),(B,(2,8))]
                            [(B,(3,1)),(W,(3,2)),(B,(3,3)),(W,(3,4)),(B,(3,5)),(W,(3,6)),(B,(3,7)),(W,(3,8))]
                            [(W,(4,1)),(B,(4,2)),(W,(4,3)),(B,(4,4)),(W,(4,5)),(B,(4,6)),(W,(4,7)),(B,(4,8))]
                            [(B,(5,1)),(W,(5,2)),(B,(5,3)),(W,(5,4)),(B,(5,5)),(W,(5,6)),(B,(5,7)),(W,(5,8))]
                            [(W,(6,1)),(B,(6,2)),(W,(6,3)),(B,(6,4)),(W,(6,5)),(B,(6,6)),(W,(6,7)),(B,(6,8))]
                            [(B,(7,1)),(W,(7,2)),(B,(7,3)),(W,(7,4)),(B,(7,5)),(W,(7,6)),(B,(7,7)),(W,(7,8))]
                            [(W,(8,1)),(B,(8,2)),(W,(8,3)),(B,(8,4)),(W,(8,5)),(B,(8,6)),(W,(8,7)),(B,(8,8))]
-} 

makeSquare :: IO ()
makeSquare = mapM_ print (letmakeRows)

{- addPosition
    adds positions to the board, first tile is (1,1), tile to the right of it is (1,2) etc. First number is row, second number is column.
   RETURNS: [(Square, Position)]
   EXAMPLES: addPosition = [(B,(1,1)),(W,(1,2)),(B,(1,3)),(W,(1,4)),(B,(1,5)),(W,(1,6)),(B,(1,7)),(W,(1,8)),(W,(2,1)),(B,(2,2)),(W,(2,3)),(B,(2,4)),(W,(2,5)),(B,(2,6)),(W,(2,7)),(B,(2,8)),(B,(3,1)),(W,(3,2)),(B,(3,3)),(W,(3,4)),(B,(3,5)),(W,(3,6)),(B,(3,7)),(W,(3,8)),(W,(4,1)),(B,(4,2)),(W,(4,3)),(B,(4,4)),(W,(4,5)),(B,(4,6)),(W,(4,7)),(B,(4,8)),(B,(5,1)),(W,(5,2)),(B,(5,3)),(W,(5,4)),(B,(5,5)),(W,(5,6)),(B,(5,7)),(W,(5,8)),(W,(6,1)),(B,(6,2)),(W,(6,3)),(B,(6,4)),(W,(6,5)),(B,(6,6)),(W,(6,7)),(B,(6,8)),(B,(7,1)),(W,(7,2)),(B,(7,3)),(W,(7,4)),(B,(7,5)),(W,(7,6)),(B,(7,7)),(W,(7,8)),(W,(8,1)),(B,(8,2)),(W,(8,3)),(B,(8,4)),(W,(8,5)),(B,(8,6)),(W,(8,7)),(B,(8,8))]
-}
addPosition::[(Square, Position)]
addPosition = let (x:xs) = makecheckeredBoard in
                  addPositionAux (x:xs) (1,1) where
{- addPositionAux Board
    adds positions to the board, first tile is (1,1), tile to the right of it is (1,2) etc. Helper function of addPosition.
   RETURNS: [(Square, Position)]
   EXAMPLES: addPositionAux makecheckeredBoard = [(B,(1,1)),(W,(1,2)),(B,(1,3)),(W,(1,4)),(B,(1,5)),(W,(1,6)),(B,(1,7)),(W,(1,8)),(W,(2,1)),(B,(2,2)),(W,(2,3)),(B,(2,4)),(W,(2,5)),(B,(2,6)),(W,(2,7)),(B,(2,8)),(B,(3,1)),(W,(3,2)),(B,(3,3)),(W,(3,4)),(B,(3,5)),(W,(3,6)),(B,(3,7)),(W,(3,8)),(W,(4,1)),(B,(4,2)),(W,(4,3)),(B,(4,4)),(W,(4,5)),(B,(4,6)),(W,(4,7)),(B,(4,8)),(B,(5,1)),(W,(5,2)),(B,(5,3)),(W,(5,4)),(B,(5,5)),(W,(5,6)),(B,(5,7)),(W,(5,8)),(W,(6,1)),(B,(6,2)),(W,(6,3)),(B,(6,4)),(W,(6,5)),(B,(6,6)),(W,(6,7)),(B,(6,8)),(B,(7,1)),(W,(7,2)),(B,(7,3)),(W,(7,4)),(B,(7,5)),(W,(7,6)),(B,(7,7)),(W,(7,8)),(W,(8,1)),(B,(8,2)),(W,(8,3)),(B,(8,4)),(W,(8,5)),(B,(8,6)),(W,(8,7)),(B,(8,8))]
-}
                      addPositionAux:: Board -> Position -> [(Square, Position)]
                      addPositionAux [] _ = []
                      addPositionAux all@(x:xs) (a,b)
                          |b < 8 = [(x,(a,b))] ++ addPositionAux xs (a,b+1)
                          |b == 8 = [(x,(a,b))] ++ addPositionAux xs (a+1,b-7)

{- insertPlayer
    inserts the pieces onto an empty board but only 12 pieces per color and only on black tiles. 
   RETURNS: [(Square, Position)]
   EXAMPLES: insertPlayer = [(P R N,(1,1)),(W,(1,2)),(P R N,(1,3)),(W,(1,4)),(P R N,(1,5)),(W,(1,6)),(P R N,(1,7)),(W,(1,8)),(W,(2,1)),(P R N,(2,2)),(W,(2,3)),(P R N,(2,4)),(W,(2,5)),(P R N,(2,6)),(W,(2,7)),(P R N,(2,8)),(P R N,(3,1)),(W,(3,2)),(P R N,(3,3)),(W,(3,4)),(P R N,(3,5)),(W,(3,6)),(P R N,(3,7)),(W,(3,8)),(W,(4,1)),(B,(4,2)),(W,(4,3)),(B,(4,4)),(W,(4,5)),(B,(4,6)),(W,(4,7)),(B,(4,8)),(B,(5,1)),(W,(5,2)),(B,(5,3)),(W,(5,4)),(B,(5,5)),(W,(5,6)),(B,(5,7)),(W,(5,8)),(W,(6,1)),(P Wh N,(6,2)),(W,(6,3)),(P Wh N,(6,4)),(W,(6,5)),(P Wh N,(6,6)),(W,(6,7)),(P Wh N,(6,8)),(P Wh N,(7,1)),(W,(7,2)),(P Wh N,(7,3)),(W,(7,4)),(P Wh N,(7,5)),(W,(7,6)),(P Wh N,(7,7)),(W,(7,8)),(W,(8,1)),(P Wh N,(8,2)),(W,(8,3)),(P Wh N,(8,4)),(W,(8,5)),(P Wh N,(8,6)),(W,(8,7)),(P Wh N,(8,8))]
-}
insertPlayer :: [(Square, Position)]
insertPlayer = let ((x,(a,b)):xs) = addPosition in
                   insertPlayerRed ((x,(a,b)):xs) (P R N) 1 where
                       insertPlayerRed [] _ _ = []
                       insertPlayerRed ((x,(a,b)):xs) (P R N) (num)
                           |x == B && num < 12 = ((P R N),(a,b)):(insertPlayerRed xs (P R N) (num+1))
                           |x == W = (x,(a,b)):(insertPlayerRed xs (P R N) (num))
                           |x == B && num == 12 = ((P R N),(a,b)):(reverse(insertPlayerWhite (reverse xs)))
                           
{- insertPlayerWhite [(Square, Position)]
    takes the rest of the board after the red pieces has been added and adds the white pieces to it. Only insert 12 white pieces and only on black tiles.  
   RETURNS: [(Square, Position)]
   EXAMPLES: insertPlayerWhite [(B, (1,2))] = [(P Wh N,(1,2))]
             insertPlayerWhite [(W, (1,2))] = [(W, (1,2))]
            

-}
insertPlayerWhite :: [(Square, Position)] -> [(Square, Position)]
insertPlayerWhite xs = insertPlayerWhiteAux xs (P Wh N) (1) where
    insertPlayerWhiteAux [] _ _ = []
    insertPlayerWhiteAux ((x,(a,b)):ys) (P Wh N) (num)
        |x == B && num < 12 = ((P Wh N),(a,b)):(insertPlayerWhiteAux ys (P Wh N) (num+1))
        |x == W = (x,(a,b)):(insertPlayerWhiteAux ys (P Wh N) (num))
        |x == B && num == 12 = ((P Wh N),(a,b)):ys

                        

{- letmakeRowswithPieces
    Literally exactly the same as letmakeRows but with pieces placed on some of the tiles. Chops the list of squares into 8 element rows.
   RETURNS: [[(Square,Position)]]
   EXAMPLES: letmakeRowswithPieces = [[(P R N,(1,1)),(W,(1,2)),(P R N,(1,3)),(W,(1,4)),(P R N,(1,5)),(W,(1,6)),(P R N,(1,7)),(W,(1,8))],[(W,(2,1)),(P R N,(2,2)),(W,(2,3)),(P R N,(2,4)),(W,(2,5)),(P R N,(2,6)),(W,(2,7)),(P R N,(2,8))],[(P R N,(3,1)),(W,(3,2)),(P R N,(3,3)),(W,(3,4)),(P R N,(3,5)),(W,(3,6)),(P R N,(3,7)),(W,(3,8))],[(W,(4,1)),(B,(4,2)),(W,(4,3)),(B,(4,4)),(W,(4,5)),(B,(4,6)),(W,(4,7)),(B,(4,8))],[(B,(5,1)),(W,(5,2)),(B,(5,3)),(W,(5,4)),(B,(5,5)),(W,(5,6)),(B,(5,7)),(W,(5,8))],[(W,(6,1)),(P Wh N,(6,2)),(W,(6,3)),(P Wh N,(6,4)),(W,(6,5)),(P Wh N,(6,6)),(W,(6,7)),(P Wh N,(6,8))],[(P Wh N,(7,1)),(W,(7,2)),(P Wh N,(7,3)),(W,(7,4)),(P Wh N,(7,5)),(W,(7,6)),(P Wh N,(7,7)),(W,(7,8))],[(W,(8,1)),(P Wh N,(8,2)),(W,(8,3)),(P Wh N,(8,4)),(W,(8,5)),(P Wh N,(8,6)),(W,(8,7)),(P Wh N,(8,8))]]
-}
letmakeRowswithPieces :: [[(Square, Position)]]                       
letmakeRowswithPieces = let (x:xs) = insertPlayer in
                  rows (x:xs) where
{- rows [(Square, Position)]
    Chops the list of squares into 8 element rows.
   RETURNS: [[(Square,Position)]]
   EXAMPLES: rows insertPlayer = [[(P R N,(1,1)),(W,(1,2)),(P R N,(1,3)),(W,(1,4)),(P R N,(1,5)),(W,(1,6)),(P R N,(1,7)),(W,(1,8))],[(W,(2,1)),(P R N,(2,2)),(W,(2,3)),(P R N,(2,4)),(W,(2,5)),(P R N,(2,6)),(W,(2,7)),(P R N,(2,8))],[(P R N,(3,1)),(W,(3,2)),(P R N,(3,3)),(W,(3,4)),(P R N,(3,5)),(W,(3,6)),(P R N,(3,7)),(W,(3,8))],[(W,(4,1)),(B,(4,2)),(W,(4,3)),(B,(4,4)),(W,(4,5)),(B,(4,6)),(W,(4,7)),(B,(4,8))],[(B,(5,1)),(W,(5,2)),(B,(5,3)),(W,(5,4)),(B,(5,5)),(W,(5,6)),(B,(5,7)),(W,(5,8))],[(W,(6,1)),(P Wh N,(6,2)),(W,(6,3)),(P Wh N,(6,4)),(W,(6,5)),(P Wh N,(6,6)),(W,(6,7)),(P Wh N,(6,8))],[(P Wh N,(7,1)),(W,(7,2)),(P Wh N,(7,3)),(W,(7,4)),(P Wh N,(7,5)),(W,(7,6)),(P Wh N,(7,7)),(W,(7,8))],[(W,(8,1)),(P Wh N,(8,2)),(W,(8,3)),(P Wh N,(8,4)),(W,(8,5)),(P Wh N,(8,6)),(W,(8,7)),(P Wh N,(8,8))]]
-}
                      rows:: [(Square, Position)] -> [[(Square, Position)]]
                      rows l@(x:xs) = [take 8 l] ++ rows (drop 8 l)
                      rows [] = []                        

{- makeSquareWithPieces
    makes the list of tiles into a readable square 8 x 8 with player pieces. Wh = White, R = Red, P = Player, N = Normal. Normal is the piece type as a piece
    can be either normal or king (Queen in swedish version). 
   RETURNS: IO()
   EXAMPLES: makeSquareWithPieces = [(P R N,(1,1)),(W,(1,2)),(P R N,(1,3)),(W,(1,4)),(P R N,(1,5)),(W,(1,6)),(P R N,(1,7)),(W,(1,8))]
                                    [(W,(2,1)),(P R N,(2,2)),(W,(2,3)),(P R N,(2,4)),(W,(2,5)),(P R N,(2,6)),(W,(2,7)),(P R N,(2,8))]
                                    [(P R N,(3,1)),(W,(3,2)),(P R N,(3,3)),(W,(3,4)),(P R N,(3,5)),(W,(3,6)),(P R N,(3,7)),(W,(3,8))]
                                    [(W,(4,1)),(B,(4,2)),(W,(4,3)),(B,(4,4)),(W,(4,5)),(B,(4,6)),(W,(4,7)),(B,(4,8))]
                                    [(B,(5,1)),(W,(5,2)),(B,(5,3)),(W,(5,4)),(B,(5,5)),(W,(5,6)),(B,(5,7)),(W,(5,8))]
                                    [(W,(6,1)),(P Wh N,(6,2)),(W,(6,3)),(P Wh N,(6,4)),(W,(6,5)),(P Wh N,(6,6)),(W,(6,7)),(P Wh N,(6,8))]
                                    [(P Wh N,(7,1)),(W,(7,2)),(P Wh N,(7,3)),(W,(7,4)),(P Wh N,(7,5)),(W,(7,6)),(P Wh N,(7,7)),(W,(7,8))]
                                    [(W,(8,1)),(P Wh N,(8,2)),(W,(8,3)),(P Wh N,(8,4)),(W,(8,5)),(P Wh N,(8,6)),(W,(8,7)),(P Wh N,(8,8))]
-} 

makeSquareWithPieces :: IO ()
makeSquareWithPieces = mapM_ print (letmakeRowswithPieces)
                        
                          
gameState = undefined

movePlayer = undefined


