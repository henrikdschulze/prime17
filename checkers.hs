import Data.Char
import Control.Monad
import Control.Exception
import System.Random
import Debug.Trace

data Square = Black | White deriving (Show,Eq)

data Pcolor = PBlack | PWhite deriving (Show)
data PType = Normal | Queen deriving (Show)
data Piece = Piece Pcolor PType deriving (Show)

type Board = [Square]

type Position = (Int, Int)

type Player = String

type Move = (Int,Int)





sizeofBoard = replicate 64 Black 

makeBoard = let (x:xs) = sizeofBoard in
                   makeBoardAux (x:xs) where
                       makeBoardAux (x:y:xs) = Black:White:makeBoardAux xs
                       makeBoardAux [] = []

gameState = undefined

movePlayer = undefined


