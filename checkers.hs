import Data.Char
import Control.Monad
import Control.Exception

data Square = Empty | White Position | Black Position

type Board = [Square]
type Position = (Int, Int)
type Player = String

type Move = (Int,Int)


