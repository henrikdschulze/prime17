import Data.Char
import Control.Monad
import Control.Exception
import System.Random
import Debug.Trace
import Data.List


data Player = Player Color Type

data Color = Red | White
data Type = Normal | Queen

type Board = [[Char]]

type Position = (Int, Int)
type Move = (Int, Int)

readPiece:: Player -> String
readPiece (Player Red Normal) = "r"
readPiece (Player Red Queen) = "R"
readPiece (Player White Normal) = "w"
readPiece (Player White Queen) = "W"


sizeofBoardish = replicate 64 "."
                          
addPosition = let (x:xs) = insertPlayer in
                  addPositionAux (x:xs) (1,1) where
                      addPositionAux [] _ = []
                      addPositionAux all@(x:xs) (a,b)
                          |b < 8 = [(x,(a,b))] ++ addPositionAux xs (a,b+1)
                          |b == 8 = [(x,(a,b))] ++ addPositionAux xs (a+1,b-7)
                          
                          
--insertPlayer :: [(Square, Position)]
-- insertPlayer = let ((x,(a,b)):xs) = addPosition in
--                    insertPlayerRed ((x,(a,b)):xs) readPiece (Player Red Normal) (1) where
--                        insertPlayerRed [] _ _ _ = []
--                        insertPlayerRed ((x,(a,b)):((y,(d,e)):xs)) readPiece (Player Red Normal) (num)
--                            |num <= 4 = (readPiece (Player Red Normal),(a,b)):(y,(d,e)):(insertPlayerRed xs readPiece (Player Red Normal) (num+1))
--                            |num > 4 && num <= 8 = (y,(d,e)):(readPiece (Player Red Normal),(a,b)):(insertPlayerRed xs readPiece (Player Red Normal) (num+1))
--                            |num > 8 && num < 12 = (readPiece (Player Red Normal),(a,b)):(y,(d,e)):(insertPlayerRed xs readPiece (Player Red Normal) (num+1))
--                            |num == 12 = (readPiece (Player Red Normal),(a,b)):(y,(d,e)):(reverse(insertPlayerWhite (reverse xs)))
-- 
-- insertPlayerWhite:: [([Char], (Integer, Integer))] -> [([Char], (Integer, Integer))]
-- 
-- insertPlayerWhite xs = insertPlayerWhiteAux xs readPiece (Player White Normal) (1) where
--     insertPlayerWhiteAux [] _ _ _ = xs
--     insertPlayerWhiteAux ((x,(a,b)):((y,(d,e)):ys)) readPiece (Player White Normal) (num)
--         |num <= 4 = (readPiece (Player White Normal),(a,b)):(y,(d,e)):(insertPlayerWhiteAux ys readPiece (Player White Normal) (num+1))
--         |num > 4 && num <= 8 = (y,(d,e)):(readPiece (Player White Normal),(a,b)):(insertPlayerWhiteAux ys readPiece (Player White Normal) (num+1))
--         |num > 8 && num <= 12 = (readPiece (Player White Normal),(a,b)):(y,(d,e)):(insertPlayerWhiteAux ys readPiece (Player White Normal) (num+1))
--         |num > 12 = (insertPlayerWhiteAux [] readPiece (Player White Normal) 1) 
        

        
insertPlayer = let (x:y:xs) = sizeofBoardish in
                   insertPlayerRed (x:y:xs) readPiece (Player Red Normal) (1) where
                       insertPlayerRed [] _ _ _ = []
                       insertPlayerRed (x:y:xs) readPiece (Player Red Normal) (num)
                           |num <= 4 = (readPiece (Player Red Normal)):y:(insertPlayerRed xs readPiece (Player Red Normal) (num+1))
                           |num > 4 && num <= 8 = y:(readPiece (Player Red Normal)):(insertPlayerRed xs readPiece (Player Red Normal) (num+1))
                           |num > 8 && num < 12 = (readPiece (Player Red Normal)):y:(insertPlayerRed xs readPiece (Player Red Normal) (num+1))
                           |num == 12 = (readPiece (Player Red Normal)):y:(reverse(insertPlayerWhite (reverse xs)))


insertPlayerWhite xs = insertPlayerWhiteAux xs readPiece (Player White Normal) (1) where
    insertPlayerWhiteAux [] _ _ _ = []
    insertPlayerWhiteAux (x:y:ys) readPiece (Player White Normal) (num)
        |num <= 4 = (readPiece (Player White Normal)):y:(insertPlayerWhiteAux ys readPiece (Player White Normal) (num+1))
        |num > 4 && num <= 8 = y:(readPiece (Player White Normal)):(insertPlayerWhiteAux ys readPiece (Player White Normal) (num+1))
        |num > 8 && num < 12 = (readPiece (Player White Normal)):y:(insertPlayerWhiteAux ys readPiece (Player White Normal) (num+1))
        |num == 12 = (readPiece (Player White Normal)):y:ys      
        
        
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
    printboard gameState



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
