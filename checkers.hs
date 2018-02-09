--MADDE
import Data.Char
import Control.Monad
import Control.Exception
import System.Random
import Debug.Trace

data Square = Black | White | PlayerRed PType | PlayerWhite PType deriving (Show,Eq)

data PType = Normal | Queen deriving (Show,Eq)


type Board = [Square]

type Position = (Int, Int)

type Move = (Int,Int)

posBoard [] = []
posBoard (x:xs) = posboards (x:xs) (0,0)

posboards (x:xs) (a,b) = (x,(a+1,b+1)): posboards (xs) (a,b)


sizeofBoard = replicate 64 Black 

makeEmptyBoard = let (x:xs) = sizeofBoard in
                   makeBoardAux (x:xs) where
                       makeBoardAux (x:y:xs) = Black:White:makeBoardAux xs
                       makeBoardAux [] = []

letmakeRows = let (x:xs) = makeEmptyBoard in
                  rows (x:xs) where
                      rows l@(x:xs) = [take 8 l] ++ rows (drop 8 l)
                      rows [] = []
                       
gameState = undefined

movePlayer = undefined

