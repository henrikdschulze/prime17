import Data.Char
import Control.Monad
import Control.Exception
import Debug.Trace
import Data.List
import Test.HUnit

{- the Player is represented by a color and a type
  INVARIANT:
    There are only three colors and three types because Player None NotHere is there to represent if
    there is no piece in that position, and that Color None and Type NotHere is there as an edge case.
    Each Color corresponds to a character and uppercase or lowercase to represent the character's type.
    Player Red Normal & Player Red King represent Player Red while Player White Normal
    & Player White King represent Player White.
 -}
data Player = Player Color Type

{- the Color is either Red, White or None.
  INVARIANT:
    There are only three colors and each Color corresponds to a character. "r" for Red and "w" for White.
    These will represent the colors of the pieces one the board. Color must be represented as a String.
    None is a ".", meaning there is no piece there.
 -}
data Color = Red | White | None

{- the Type is either Normal, King or NotHere.
  INVARIANT:
    There are only three types. Lowercase characters represent "Normal" pieces and uppercase characters
    represent "King" pieces, meaning for example that player Red have managed to move one of the pieces
    to the last row. "r" for Red Normal, "R" for Red King and "w" for White Normal and "W" for White King.
    NotHere means that the space is empty, namely ".".
-}
data Type = Normal | King | NotHere

{- GameState is the board and is represented as a list of strings in a list.
  INVARIANT:
    The GameState is never to be empty and should always be 8x8 in size. No numbers should be in the GameState.
 -}
type GameState = [[String]]

{- GameStateWithPosition is the board with positions, where the position is represented as a tuple
    with the first number being the row number, and the second number being the column number.
  INVARIANT:
    The GameState is never to be empty and should always be 8x8 in size. The positions can't be Floats.
 -}
type GameStateWithPosition = [[(String,Position)]]

{- ListStateWithPosition is the a concatenation of the board with positions. This version is needed
    to find specific elements in the list.
  INVARIANT:
    The positions can't be Floats. The ListwithPosition should always have the same number of elements
    as the GameState.
 -}
type ListwithPosition = [(String,Position)]

{- List is a list of strings. It's basically the board but with every element in one row instead.
  INVARIANT:
    Only Strings, also the list should never be empty.
 -}
type List = [String]

{- The Position is represented as a tuple with the first number being the row number and the second number
    being the column number.
  INVARIANT:
    row number are 1-8. Column numbers are 1-8. No numbers outside these are valid.
 -}
type Position = (Int, Int)

{- Move is represented as a tuple with the first number being the row number and the second number
    being the column number.
  INVARIANT:
    row number are 1-8. Column numbers are 1-8. No numbers outside these are valid.
 -}
type Move = (Int, Int)

{- readPiece Player
    Converts the Player data type to a String.
  PRE:     -
  RETURNS: the Player data type represented as a string.
  SIDE EFFECTS: -
  EXAMPLES:
    readPiece (Player Red Normal)
      == "r"
    readPiece (Player White Normal)
      == "w"
-}
readPiece:: Player -> String
readPiece (Player Red Normal) = "r"
readPiece (Player Red King) = "R"
readPiece (Player White Normal) = "w"
readPiece (Player White King) = "W"
readPiece (Player None NotHere) = "."

{- showPiece String
    Takes a string and returns the same string. Useful when you are not sure
    what string it is in some position in the gamestate.
  PRE:     -
  RETURNS: String
  SIDE EFFECTS: -
  EXAMPLES:
    showPiece "w" = "w"
    showPiece "r" = "r"
-}
showPiece :: String -> String
showPiece "w" = readPiece (Player White Normal)
showPiece "W" = readPiece (Player White King)
showPiece "r" = readPiece (Player Red Normal)
showPiece "R" = readPiece (Player Red King)
showPiece "." = readPiece (Player None NotHere)

{- upgradePiece String
    Takes a string and returns the string in uppercase. If the string already is in uppercase,
    nothing happens, it just returns the same string.
  PRE:     -
  RETURNS: String
  SIDE EFFECTS: -
  EXAMPLES:
    upgradePiece "r" = "R"
    upgradePiece "w" = "W"
-}
upgradePiece:: String -> String
upgradePiece "r" = showPiece "R"
upgradePiece "w" = showPiece "W"
upgradePiece "W" = showPiece "W"
upgradePiece "R" = showPiece "R"

{- emptyBoard
    Makes an "empty board" with 64 elements. It's filled with "." to signify that there are
    no pieces at any position.
  PRE:     -
  RETURNS: List
  SIDE EFFECTS: -
  EXAMPLES:
    emptyBoard
  == [".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".","."]
-}
emptyBoard::List
emptyBoard = replicate 64 "."

{- addPosition
    Adds positions to the elements in the list. Row number is the first number and goes from 1 to 8.
    Column number is the second number and goes from 1 to 8.
  PRE:     -
  RETURNS: ListwithPosition
  SIDE EFFECTS: -
  EXAMPLES:
    addPosition
  == [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),
      (".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),
      ("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),
      (".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),
      (".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),
      (".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),
      ("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),
      (".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))]
-}
addPosition::ListwithPosition
addPosition = let (x:xs) = insertPlayer in addPositionAux (x:xs) (1,1)
{- addPositionAux List Position
    Adds positions to the elements in the list. Row number is the first number and goes from 1 to 8.
    Column number is the second number and goes from 1 to 8.
  PRE:     -
  RETURNS: ListwithPosition
  SIDE EFFECTS: -
  EXAMPLES:
    addPositionAux [] _
      == []
    addPositionAux ["r","."] (1,1)
      == [("r",(1,1)),(".",(1,2))]
-}
addPositionAux:: List -> Position -> ListwithPosition
addPositionAux [] _ = []
addPositionAux all@(x:xs) (a,b)
  | b < 8 = [(x,(a,b))] ++ addPositionAux xs (a,b+1)
  | b == 8 = [(x,(a,b))] ++ addPositionAux xs (a+1,b-7)

{- removePosition ListwithPosition
    removes the positions from the list, returning only a list with the empty tiles and pieces.
  PRE:     -
  RETURNS: List
  SIDE EFFECTS: -
  EXAMPLES:
    removePosition []
      == []
    removePosition [("r",(1,1)),(".",(1,2))]
      == ["r","."]
-}
removePosition :: ListwithPosition -> List
removePosition [] = []
removePosition ((x,(a,b)):xs) = [x] ++ removePosition xs

{- addPositionGameState GameState
    Adds positions to the GameState. Row number is the first number and goes from 1 to 8.
    Column number is the second number and goes from 1 to 8.
  PRE:     No empty list
  RETURNS: GameStateWithPosition
  SIDE EFFECTS: -
  EXAMPLES:
    addPositionGameState [["r","."],["r"]]
      == [[("r",(1,1)),(".",(1,2)),("r",(1,3))]]
-}
addPositionGameState :: GameState -> GameStateWithPosition
addPositionGameState (x:xs) = makeGamestate (addPositionAux (concat(x:xs)) (1,1)) where
  {- addPositionAux List Position
      Adds positions to the elements in the list. Row number is the first number and goes from 1 to 8.
      Column number is the second number and goes from 1 to 8.
    PRE:     -
    RETURNS: ListwithPosition
    SIDE EFFECTS: -
    EXAMPLES:
      addPositionAux [] _
        == []
      addPositionAux ["r","."] (1,1)
        == [("r",(1,1)),(".",(1,2))]
  -}
  addPositionAux:: List -> Position -> ListwithPosition
  addPositionAux [] _ = []
  addPositionAux all@(x:xs) (a,b)
    | b < 8 = [(x,(a,b))] ++ addPositionAux xs (a,b+1)
    | b == 8 = [(x,(a,b))] ++ addPositionAux xs (a+1,b-7)

{- insertPlayer
    Insert the player pieces into the empty list.
  PRE:     -
  RETURNS: List
  SIDE EFFECTS: -
  EXAMPLES:
    insertPlayer
  == ["r",".","r",".","r",".","r",".",
      ".","r",".","r",".","r",".","r",
      "r",".","r",".","r",".","r",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".","w",".","w",".","w",".","w",
      "w",".","w",".","w",".","w",".",
      ".","w",".","w",".","w",".","w"]
-}
insertPlayer::List
insertPlayer = let (x:y:xs) = emptyBoard in insertPlayerRed (x:y:xs) readPiece (Player Red Normal) (1) where
{- insertPlayerRed List readPiece Player Int
    Insert the player Red pieces into the empty list as strings.
    After that it calls the insertPlayerWhite function and concatinates itself with that list.
  PRE:     List must have an even number of elements
  RETURNS: List
  SIDE EFFECTS: -
  EXAMPLES:
    insertPlayerRed emptyBoard readPiece (Player Red Normal) 1
  == ["r",".","r",".","r",".","r",".",
      ".","r",".","r",".","r",".","r",
      "r",".","r",".","r",".","r",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".","w",".","w",".","w",".","w",
      "w",".","w",".","w",".","w",".",
      ".","w",".","w",".","w",".","w"]
-}
insertPlayerRed :: (Ord a, Num a) => [String] -> (Player -> String) -> Player -> a -> [String]
insertPlayerRed [] _ _ _ = []
insertPlayerRed (x:y:xs) readPiece (Player Red Normal) (num)
  | num <= 4 = (readPiece (Player Red Normal)):y:(insertPlayerRed xs readPiece (Player Red Normal) (num+1))
  | num > 4 && num <= 8 = y:(readPiece (Player Red Normal)):(insertPlayerRed xs readPiece (Player Red Normal) (num+1))
  | num > 8 && num < 12 = (readPiece (Player Red Normal)):y:(insertPlayerRed xs readPiece (Player Red Normal) (num+1))
  | num == 12 = (readPiece (Player Red Normal)):y:(reverse(insertPlayerWhite (reverse xs)))

{- insertPlayerWhite List
    Insert the player White pieces into a list.
  PRE:     List must have an even number of elements
  RETURNS: List
  SIDE EFFECTS: -
  EXAMPLES:
    insertPlayerWhite ["r",".","r"]
      *** Exception: checkers.hs:(203,5)-(208,59): Non-exhaustive patterns in function insertPlayerWhiteAux
    insertPlayerWhite ["r",".","r","."]
      == ["w",".","w","."]
-}
insertPlayerWhite:: List -> List
insertPlayerWhite xs = insertPlayerWhiteAux xs readPiece (Player White Normal) (1) where
{- insertPlayerWhiteAux List readPiece Player Int
    Insert the player White pieces into the empty list as strings.
  PRE:     List must have an even number of elements
  RETURNS: List
  SIDE EFFECTS: -
  EXAMPLES:
    insertPlayerWhiteAux emptyBoard readPiece (Player White Normal) 1
  == ["w",".","w",".","w",".","w",".",
      ".","w",".","w",".","w",".","w",
      "w",".","w",".","w",".","w",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".","."]
-}
insertPlayerWhiteAux :: (Ord a, Num a) => [String] -> (Player -> String) -> Player -> a -> [String]
insertPlayerWhiteAux [] _ _ _ = []
insertPlayerWhiteAux (x:y:ys) readPiece (Player White Normal) (num)
  | num <= 4            = (readPiece (Player White Normal)):y:(insertPlayerWhiteAux ys readPiece (Player White Normal) (num+1))
  | num > 4 && num <= 8 = y:(readPiece (Player White Normal)):(insertPlayerWhiteAux ys readPiece (Player White Normal) (num+1))
  | num > 8 && num < 12 = (readPiece (Player White Normal)):y:(insertPlayerWhiteAux ys readPiece (Player White Normal) (num+1))
  | num == 12           = (readPiece (Player White Normal)):y:ys

{- makeGamestate [a]
    Makes a list into a list of lists
  PRE:     No empty list
  RETURNS: [[a]]
  SIDE EFFECTS: -
  EXAMPLES:
    makeGamestate ["r",".",".",".",".",".",".",".","."]
      == [["r",".",".",".",".",".",".","."],["."]]
-}
makeGamestate:: [a] -> [[a]]
makeGamestate (x:xs) = rows (x:xs) where
{- rows [a]
    Makes a list into a list of lists
    VARIANT: Length of list
    PRE: True
    RETURNS: [[a]]
    EXAMPLES: rows ["r",".",".",".",".",".",".",".","."] = [["r",".",".",".",".",".",".","."],["."]]
-}
    rows l@(x:xs) = [take 8 l] ++ rows (drop 8 l)
    rows [] = []

{- genGameState
    Generates the starting game state
  PRE:     None
  RETURNS: IO GameState
  SIDE EFFECTS: -
  EXAMPLES: -
-}
genGameState:: IO GameState
genGameState = do
    return (makeGamestate (insertPlayer))

{- main
    Starts the game and generates the gameState
  PRE:     None
  RETURNS: IO ()
  SIDE EFFECTS: Prints "Welcome to Checkers" on the screen
  EXAMPLES: -
-}
main :: IO ()
main = do
  putStrLn "Welcome to Checkers."
  gameState <- genGameState
  printboard gameState
  play gameState

{- play
    Prints out the current gamestate, generates new gamestates from the moves from both players,
  and finally checks victory conditions
  PRE:     Valid gameState
  RETURNS: IO ()
  SIDE EFFECTS: Prints out gameState on screen. If victory is achieved then prints out message to player.
  EXAMPLES: -
-}
play :: GameState -> IO ()
play gameState = do
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
    Checks if the last move caused the red piece to land on row 8, making it valid to become a King piece.
  PRE:     -
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES:
    checkifcanUpgradeRed (8,1) = True
          checkifcanUpgradeRed (5,5) = False
-}
checkifcanUpgradeRed :: Position -> Bool
checkifcanUpgradeRed (a,b)
    | a == 8 = True
    | otherwise = False

 {- checkifcanUpgradeWhite Position
    Checks if the last move caused the white piece to land on row 1, making it valid to become a King piece.
  PRE:     -
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES:
    checkifcanUpgradeWhite (1,1) = True
          checkifcanUpgradeWhite (4,5) = False
-}
checkifcanUpgradeWhite :: Position -> Bool
checkifcanUpgradeWhite (a,b)
    | a == 1 = True
    | otherwise = False

 {- upgradeRed gameState position
    Upgrades the red piece to king. Checks one more time that it is at the correct position and
    finds the piece corresponding to that position.
  PRE:     gameState must not be an empty list and the position must be valid. The list must be
          large enough to include the position checked for because of the index operation (!!).
  RETURNS: GameState
  SIDE EFFECTS: -
  EXAMPLES:
    upgradeRed [["r",".","r",".","r",".","r","."],
                [".","r",".","r",".","r",".","r"],
                ["r",".","r",".","r",".","r","."],
                [".",".",".",".",".",".",".","."],
                [".",".",".",".",".",".",".","."],
                [".","w",".","w",".","w",".","w"],
                ["w",".","w",".","w",".","w","."],
                [".","w",".","w",".","w",".","r"]] (8,8)
  == [["r",".","r",".","r",".","r","."],
      [".","r",".","r",".","r",".","r"],
      ["r",".","r",".","r",".","r","."],
      [".",".",".",".",".",".",".","."],
      [".",".",".",".",".",".",".","."],
      [".","w",".","w",".","w",".","w"],
      ["w",".","w",".","w",".","w","."],
      [".","w",".","w",".","w",".","R"]]
-}
upgradeRed :: GameState -> Position -> GameState
upgradeRed newGameState (a,b)
 = makeGamestate (removePosition (upgradeAux (concat (addPositionGameState newGameState)) (a,b))) where
  {- upgradeAux listwithPosition position
      Upgrades the red piece to king. Checks one more time that it is at the correct position and
      finds the piece corresponding to that position.
    PRE:     listwithPosition must not be an empty list and the position must be valid. The list must
            be large enough to include the position checked for because of the index operation (!!).
    RETURNS: ListwithPosition
    SIDE EFFECTS: -
    EXAMPLES:
      upgradeAux
      [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),
       (".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),
       ("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),
       (".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),
       (".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),
       (".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),
       ("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),
       (".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("r",(8,8))] (8,8)
    ==
      [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),
       (".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),
       ("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),
       (".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),
       (".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),
       (".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),
       ("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),
       (".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("R",(8,8))]
  -}
  upgradeAux :: ListwithPosition -> Position -> ListwithPosition
  upgradeAux ((y,(c,d)):ys) (a,b) =
      let (r,(s,t)) = ((y,(c,d)):ys) !! (findPosition (a,b))
      in checkUpgrade ((y,(c,d)):ys) (r,(s,t)) where
    {- checkUpgrade ListwithPosition (String,Position)
        Upgrades the red piece to king. Checks one more time that it is at the correct position.
      PRE:     the first list must not be an empty list and the position (a,b) must be valid.
      RETURNS: ListwithPosition
      SIDE EFFECTS: -
      EXAMPLES:
        checkUpgrade
        [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),
         (".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),
         ("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),
         (".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),
         (".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),
         (".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),
         ("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),
         (".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("r",(8,8))] ("r,"(8,8))
      ==
        [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),
         (".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),
         ("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),
         (".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),
         (".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),
         (".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),
         ("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),
         (".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("R",(8,8))]
    -}
    checkUpgrade:: ListwithPosition -> (String,Position) -> ListwithPosition
    checkUpgrade all@((y,(c,d)):ys) (r,(s,t))
      | s == 8 = insertAt ((upgradePiece r),(s,t)) (delete (r,(s,t)) all)
      | otherwise = ((y,(c,d)):ys)

{- upgradeWhite gameState position
    Upgrades the white piece to queen. Checks one more time that it is at the correct position
    and finds the piece corresponding to that position.
  PRE:     gameState must not be an empty list and the position must be valid. The list must be
          large enough to include the position checked for because of the index operation (!!).
  RETURNS: GameState
  SIDE EFFECTS: -
  EXAMPLES: upgradeWhite [["w","."]] (1,1) = [["W","."]]
-}
upgradeWhite:: GameState -> Position -> GameState
upgradeWhite newGameState (a,b)
 = makeGamestate (removePosition (upgradeAux (concat (addPositionGameState newGameState)) (a,b))) where
  upgradeAux ((y,(c,d)):ys) (a,b) =
    let (r,(s,t)) = ((y,(c,d)):ys) !! (findPosition (a,b)) in
    checkUpgrade ((y,(c,d)):ys) (r,(s,t)) where
  {- checkUpgrade ListwithPosition (String,Position)
      upgrade the white piece to queen. Checks one more time that it is at the correct position.
    PRE: No empty list and valid Position.
    RETURNS: ListwithPosition
    SIDE EFFECTS: -
    EXAMPLES:
      checkUpgrade 
      [("w",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),
       (".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),
       ("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),
       (".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),
       (".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),
       (".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),
       ("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),
       (".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("r",(8,8))] ("w,"(1,1))
    ==
      [("W",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),
       (".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),
       ("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),
       (".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),
       (".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),
       (".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),
       ("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),
       (".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("R",(8,8))]
  -}
  checkUpgrade all@((y,(c,d)):ys) (r,(s,t))
    | s == 1 = insertAt ((upgradePiece r),(s,t)) (delete (r,(s,t)) all)
    | otherwise = ((y,(c,d)):ys)

{- playerMoveRed GameState
    From the current gamestate, gets input from the player for one position the player want to move
    from and one position to move to and prints these to the screen. Prints out the current gamestate
    for the player to see. Also checks if certain conditions are met and if so, does those functions.
  PRE: Valid gameState
  RETURNS: IO GameState
  SIDE EFFECTS: Prints out different gameStates to the screen depending on conditions.
  EXAMPLES: -
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
                                      if checkPositionRed newGameState move1 move2 then do
                                         doubleMoveRed newGameState move2 else
                                         return $ playMove gameState move1 move2
    else do
      putStrLn "Invalid Move. You can only move your own pieces and move diagonally"
      playerMoveRed gameState

{- playerMoveWhite GameState
    From the current gamestate, gets input from the player for one position the player want to move
    from and one position to move to and prints these to the screen.
    Prints out the current gamestate for the player to see. Also checks if certain conditions
    are met and if so, does those functions.
  PRE: Valid gameState
  RETURNS: IO GameState
  SIDE EFFECTS: Prints out different gameStates to the screen depending on conditions.
  EXAMPLES: -
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
       printboard newsGameState
       return $ newsGameState
                                       else do
                                          if checkPositionwhite newGameState move1 move2 then do
                                            printboard newGameState
                                            doubleMoveWhite newGameState move2 else do
                                                printboard newGameState
                                                return $ playMove gameState move1 move2
  else do
     putStrLn "Invalid Move. You can only move your own pieces and move diagonally"
     playerMoveWhite gameState

{- doubleMoveWhite GameState Move
    If after a jump, the player is able to jump again, this functions starts forcing the player
    to move the same piece.
  PRE: Valid gameState and valid Move
  RETURNS: IO GameState
  SIDE EFFECTS: Prints out different gameStates to the screen depending on conditions
                and returns the gamestate to the playerMoveWhite function.
  EXAMPLES: -
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
    if checkPositionwhite newnewGameState move3 move4 then do
      doubleMoveWhite newnewGameState move4 else return $ playMove newGameState move3 move4
    else do
      putStrLn "Invalid Move. You can only move your own pieces and move diagonally"
      doubleMoveWhite newGameState (a,b)

{- doubleMoveRed GameState Move
    If after a jump, the player is able to jump again, this functions starts forcing the player
    to move the same piece.
  PRE: Valid gameState and valid Move
  RETURNS: IO GameState
  SIDE EFFECTS: Prints out different gameStates to the screen depending on conditions
                and returns the gamestate to the playerMoveRed function.
  EXAMPLES: -
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
    if checkPositionRed newnewGameState move3 move4 then do
      doubleMoveRed newnewGameState move4 else return $ playMove newGameState move3 move4
    else do
      putStrLn "Invalid Move. You can only move your own pieces and move diagonally"
      doubleMoveRed newGameState (a,b)

{- readMove
  Reads a move from standard input
  Post: A move object
  SIDE EFFECTS: Reads one or more lines from standard input
-}
readMove :: (IO Move) -- reads input from
readMove = do
  catch (do
    line <- getLine
    evaluate (read line))  -- evaluate required to force conversion of line to Move
    ((\_ -> do  -- exception handler
    putStrLn "Invalid input. Correct format: (row,column)"
    readMove) :: SomeException -> IO Move)

{- validMoveRed GameState Move Move
    Returns True if the moves are valid for Player Red, returns false otherwise. First move is
    the position the piece moves from. Second move it the position the player wants to move to.
  PRE: No empty gamestate
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES:
    validMoveRed (makeGamestate insertPlayer) (3,3) (4,4)
      == True
-}
validMoveRed :: GameState -> Move -> Move -> Bool
validMoveRed (x:xs) (u,v) (w,q) = validMoveRedAux (concat (addPositionGameState (x:xs))) (u,v) (w,q)
{- validMoveRedAux ListwithPosition Move Move
    Returns True if the moves are valid for Player Red, returns false otherwise. First move is
    the position the piece moves from. Second move it the position the player wants to move to.
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board.
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES:
    validMoveRedAux addPosition (3,3) (4,4)
  == True
    validMoveRedAux addPosition (3,3) (5,5)
  == False
-}
validMoveRedAux ::ListwithPosition -> Move -> Move -> Bool
validMoveRedAux [] _ _ = False
validMoveRedAux ys (u,v) (w,q) =
  let (p,(l,e)) = ys !! (findPosition (u+1,v+1)) in
  let (m,(i,o)) = ys !! (findPosition (u+1,v-1)) in
  let (a,(b,n)) = ys !! (findPosition (u-1,v+1)) in
  let (s,(f,g)) = ys !! (findPosition (u-1,v-1)) in
  let (dt,(ro,co)) = ys !! (findPosition (u+2,v+2)) in
  let (dy,(row,col)) = ys !! (findPosition (u+2,v-2)) in
  let (dx,(ry,cy)) = ys !! (findPosition (u-2,v+2)) in
  let (dq,(ru,cu)) = ys !! (findPosition (u-2,v-2)) in
  validMoveRedAuxAux ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))

{- validMoveRedAuxAux ListwithPosition Move Move (String, Position) (String, Position) (String, Position)
(String, Position) (String, Position) (String, Position) (String, Position) (String, Position)
    Returns True if the moves are valid for Player Red, returns false otherwise. First move is
    the position the piece moves from. Second move it the position the player wants to move to.
    All (String,Position) corresponds to various positions and strings around the first Move.
    The positions in order are, if we consider that first move are in the center:
    LowerRight, LowerLeft, UpperRight, Upperleft,
    LowerRightLowerRight, LowerLeftLowerLeft, UpperRightUpperRight, UpperLeftUpperLeft.
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board.
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES:
  EXAMPLES:
    validMoveRedAuxAux addPosition (3,3) (4,4)
   (".",(4,4)) (".",(4,2)) ("r",(2,4)) ("r",(2,2)) (".",(5,5)) (".",(5,1)) ("r",(1,5)) ("r",(1,1))
  == True
-}


validMoveRedAuxAux:: ListwithPosition -> Move -> Move -> (String, Position) -> (String,Position)
                      -> (String,Position) -> (String,Position) -> (String,Position)
                      -> (String,Position) -> (String,Position) -> (String, Position) -> Bool
validMoveRedAuxAux ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
  | (u,v) == (w-2,q-2) =
    validMoveRedJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
  | (u,v) == (w-2,q+2) =
    validMoveRedJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
  | (u,v) == (w+2,q+2) =
    validMoveRedJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
  | (u,v) == (w+2,q-2) =
    validMoveRedJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
  | otherwise          = validMoveAll ys (u,v) (w,q)

{- validMoveRedJump ListwithPosition Move Move (String, Position) (String, Position) (String, Position)
 (String, Position) (String, Position) (String, Position) (String, Position) (String, Position)
    Returns True if the moves are valid for Player Red, returns false otherwise. First move is
    the position the piece moves from. Second move it the position the player wants to move to.
    All (String,Position) corresponds to various positions and strings around the first Move.
    The positions in order are, if we consider that first move are in the center:
    LowerRight, LowerLeft, UpperRight, Upperleft,
    LowerRightLowerRight, LowerLeftLowerLeft, UpperRightUpperRight, UpperLeftUpperLeft.
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board.
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES:
    validMoveRedJump addPosition (3,3) (5,5)
   (".",(4,4)) (".",(4,2)) ("r",(2,4)) ("r",(2,2)) (".",(5,5)) (".",(5,1)) ("r",(1,5)) ("r",(1,1))
  == False
-}                                      
validMoveRedJump:: ListwithPosition -> Move -> Move -> (String, Position) -> (String,Position)
                    -> (String,Position) -> (String,Position) -> (String,Position)
                    -> (String,Position) -> (String,Position) -> (String, Position) -> Bool
validMoveRedJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
    | (u == 1 || u == 2) && (v == 1 || v == 2) = if ((p == "w" || p == "W") && dt == ".") then True else False
    | (u == 1 || u == 2) && (v == 7 || v == 8) = if ((m == "w" || m == "W") && dy == ".") then True else False
    | (u == 7 || u == 8) && (v == 1 || v == 2) = if ((a == "w" || a == "W") && dx == ".") then True else False
    | (u == 7 || u == 8) && (v == 7 || v == 8) = if ((s == "w" || s == "W") && dq == ".") then True else False
    | (u == 1 || u == 2) && (v >= 3 && v <= 6) = if ((p == "w" || p == "W") && dt == ".") || ((m == "w" || m == "W") && dy == ".") then True else False
    | (u >= 3 && u <= 6) && (v == 1 || v == 2) = if ((a == "w" || a == "W") && dx == ".") || ((p == "w" || p == "W") && dt == ".") then True else False
    | (u == 7 || u == 8) && (v >=3  && v <= 6) = if ((s == "w" || s == "W") && dq == ".") || ((a == "w" || a == "W") && dx == ".") then True else False
    | (u >= 3 && u <= 6) && (v == 7 || v == 8) = if ((s == "w" || s == "W") && dq == ".") || ((m == "w" || m == "W") && dy == ".") then True else False
    | otherwise = if ((p == "w" || p == "W") && dt == ".") || ((m == "w" || m == "W") && dy == ".") || 
      ((a == "w" || a == "W") && dx == ".") || ((s == "w" || s == "W") && dq == ".") then True else False

{- validMoveWhite GameState Move Move
    Returns True if the moves are valid for Player White, returns false otherwise. First move is
    the position the piece moves from. Second move it the position the player wants to move to.
  PRE: No empty gamestate
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES:
    validMoveWhite (makeGamestate insertPlayer) (6,6) (5,5)
      == True
-}
validMoveWhite :: GameState -> Move -> Move -> Bool
validMoveWhite (x:xs) (u,v) (w,q) = validMoveauxWhite (concat (addPositionGameState (x:xs))) (u,v) (w,q)
{- validMoveauxWhite ListwithPosition Move Move
    Returns True if the moves are valid for Player White, returns false otherwise. First move is
    the position the piece moves from. Second move it the position the player wants to move to.
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board.
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES: validMoveauxWhite addPosition (7,7) (8,8) = False
            validMoveauxWhite addPosition (6,6) (5,5) = True
-}
validMoveauxWhite ::ListwithPosition -> Move -> Move -> Bool
validMoveauxWhite [] _ _ = False
validMoveauxWhite ys (u,v) (w,q) =
  let (p,(l,e)) = ys !! (findPosition (u+1,v+1)) in
  let (m,(i,o)) = ys !! (findPosition (u+1,v-1)) in
  let (a,(b,n)) = ys !! (findPosition (u-1,v+1)) in
  let (s,(f,g)) = ys !! (findPosition (u-1,v-1)) in
  let (dt,(ro,co)) = ys !! (findPosition (u+2,v+2)) in
  let (dy,(row,col)) = ys !! (findPosition (u+2,v-2)) in
  let (dx,(ry,cy)) = ys !! (findPosition (u-2,v+2)) in
  let (dq,(ru,cu)) = ys !! (findPosition (u-2,v-2)) in
  validMoveWhiteAuxAux ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu)) where
  {- validMoveWhiteAuxAux ListwithPosition Move Move (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position)
      Checks first move with second move to see if you want to jump or take a step. If you want to jump it runs the function validMoveWhiteJump. Otherwise runs validMoveAll.
      All (String,Position) corresponds to various positions and strings around the first Move. The positions in order are, if we consider that first move are in the center,
      LowerRight, LowerLeft, UpperRight,Upperleft, LowerRightLowerRight, LowerLeftLowerLeft, UpperRightUpperRight, UpperLeftUpperLeft.
    PRE: Valid move (row 1-8, column 1-8), no numbers outside the board.
    RETURNS: Bool
    SIDE EFFECTS: -
    EXAMPLES:
    EXAMPLE: validMoveWhiteAuxAux addPosition (6,2) (5,3)
                ("w",(7,3)) ("w",(7,1)) (".",(5,3)) (".",(5,1)) ("w",(8,4)) (".",(7,8)) (".",(4,4)) (".",(3,8)) = True
-}
  validMoveWhiteAuxAux:: ListwithPosition -> Move -> Move -> (String, Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String, Position) -> Bool
  validMoveWhiteAuxAux ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
     | (u,v) == (w-2,q-2) = validMoveWhiteJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
     | (u,v) == (w-2,q+2) = validMoveWhiteJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
     | (u,v) == (w+2,q+2) = validMoveWhiteJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
     | (u,v) == (w+2,q-2) = validMoveWhiteJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
     | otherwise = validMoveAll ys (u,v) (w,q)

{- validMoveAll ListwithPosition Move Move
    Returns true if you have a valid move were you move your piece one step diagonally from the starting position (first move). Works for both players.
    VARIANT: Length of list
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board.
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES: validMoveAll addPosition (3,3) (4,4) = True
              validMoveAll addPosition (3,3) (8,8) = False
-}
validMoveAll :: ListwithPosition -> Move -> Move -> Bool
validMoveAll [] _ _ = False
validMoveAll ((y,(c,d)):ys) (u,v) (w,q)
    | (u,v) == (w-1,q-1) = if (c,d) == (w,q) && y == "." then True else validMoveAll ys (u,v) (w,q)
    | (u,v) == (w-1,q+1) = if (c,d) == (w,q) && y == "." then True else validMoveAll ys (u,v) (w,q)
    | (u,v) == (w+1,q+1) = if (c,d) == (w,q) && y == "." then True else validMoveAll ys (u,v) (w,q)
    | (u,v) == (w+1,q-1) = if (c,d) == (w,q) && y == "." then True else validMoveAll ys (u,v) (w,q)
    | otherwise = False

{- validMoveWhiteJump ListwithPosition Move Move (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position)
    Returns True if the moves are valid for Player White, returns false otherwise. First move is the position the piece moves from. Second move it the position the player wants to move to.
    This function considers a valid move a jump, that is to say two steps diagonally from the starting position (first move argument).
    All (String,Position) corresponds to various positions and strings around the first Move. The positions in order are, if we consider that first move are in the center,
    LowerRight, LowerLeft, UpperRight,Upperleft, LowerRightLowerRight, LowerLeftLowerLeft, UpperRightUpperRight, UpperLeftUpperLeft.
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. Valid (String,Position) around the first move according to the order above.
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES: validMoveWhiteJump addPosition (6,2) (4,4) ("w",(7,3)) ("w",(7,1)) (".",(5,3)) (".",(5,1)) ("w",(8,4)) (".",(7,8)) (".",(4,4)) (".",(3,8) = False
-}
validMoveWhiteJump:: ListwithPosition -> Move -> Move -> (String, Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String, Position) -> Bool
validMoveWhiteJump ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
    | (u == 1 || u == 2) && (v == 1 || v == 2) = if ((p == "r" || p == "R") && dt == ".") then True else False
    | (u == 1 || u == 2) && (v == 7 || v == 8) = if ((m == "r" || m == "R") && dy == ".") then True else False
    | (u == 7 || u == 8) && (v == 1 || v == 2) = if ((a == "r" || a == "R") && dx == ".") then True else False
    | (u == 7 || u == 8) && (v == 7 || v == 8) = if ((s == "r" || s == "R") && dq == ".") then True else False
    | (u == 1 || u == 2) && (v >= 3 && v <= 6) = if ((p == "r" || p == "R") && dt == ".") || ((m == "r" || m == "R") && dy == ".") then True else False
    | (u >= 3 && u <= 6) && (v == 1 || v == 2) = if ((a == "r" || a == "R") && dx == ".") || ((p == "r" || p == "R") && dt == ".") then True else False
    | (u == 7 || u == 8) && (v >=3 && v <= 6) = if ((s == "r" || s == "R") && dq == ".") || ((a == "r" || a == "R") && dx == ".") then True else False
    | (u >= 3 && u <= 6) && (v == 7 || v == 8) = if ((s == "r" || s == "R") && dq == ".") || ((m == "r" || m == "R") && dy == ".") then True else False
    | otherwise = if ((p == "r" || p == "R") && dt == ".") || ((m == "r" || m == "R") && dy == ".") || ((a == "r" || a == "R") && dx == ".") || ((s == "r" || s == "R") && dq == ".") then True else False

{- validchoiceRed GameState Move
    Returns true if you have a valid choice of player piece. That means that the player Red may only move red pieces. Move in this case is the position you want to move from.
    Note that this function only cares that you select one correct piece, it does not care if you can move from there or not.
  VARIANT: Length of list
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board.
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES: validchoiceRed (makeGamestate insertPlayer) (3,3) = True
              validchoiceRed (makeGamestate insertPlayer) (4,4) = False
-}
validchoiceRed :: GameState -> Move -> Bool
validchoiceRed (x:xs) (s,t) = validPlaceRedaux (concat (addPositionGameState (x:xs))) (s,t) where
  {- validPlaceRedaux ListwithPosition Move
      Returns true if you have a valid choice of player piece. That means that the player Red may only move red pieces. Move in this case is the position you want to move from.
      It checks which position you want to move from and checks which piece is at that position. If the piece is "r" or "R" then True. Else False.
      Note that this function only cares that you select one correct piece, it does not care if you can move from there or not.
      VARIANT: Length of list
    PRE: Valid move (row 1-8, column 1-8), no numbers outside the board.
    RETURNS: Bool
    SIDE EFFECTS: -
    EXAMPLES: validPlaceRedaux addPosition (1,1) = True
  -}
  validPlaceRedaux [] _ = False
  validPlaceRedaux ((y,(c,d)):ys) (s,t)
    | c == s && d == t = if (y == "r" || y == "R") then True else False
    | otherwise = validPlaceRedaux ys (s,t)

{- validchoiceWhite GameState Move
    Returns true if you have a valid choice of player piece. That means that the player White may only move white pieces. Move in this case is the position you want to move from.
    Note that this function only cares that you select one correct piece, it does not care if you can move from there or not. 
  VARIANT: Length of list
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
  RETURNS: Bool
  EXAMPLES: validchoiceWhite (makeGamestate insertPlayer) (5,5) = False
              validchoiceWhite (makeGamestate insertPlayer) (8,8) = True
-}  
validchoiceWhite :: GameState -> Move -> Bool
validchoiceWhite (x:xs) (s,t) = validPlaceWhiteaux (concat (addPositionGameState (x:xs))) (s,t) where
  {- validPlaceWhiteaux ListwithPosition Move
      Returns true if you have a valid choice of player piece. That means that the player White may only move white pieces. Move in this case is the position you want to move from.
      It checks which position you want to move from and checks which piece is at that position. If the piece is "w" or "W" then True. Else False. 
      Note that this function only cares that you select one correct piece, it does not care if you can move from there or not. 
    VARIANT: Length of list
    PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
    RETURNS: Bool
    EXAMPLES: validPlaceWhiteaux addPosition (7,7) = True
  -}
  validPlaceWhiteaux [] _ = False
  validPlaceWhiteaux ((y,(c,d)):ys) (s,t)
    | c == s && d == t = if (y == "w" || y == "W") then True else False
    | otherwise = validPlaceWhiteaux ys (s,t)

{- playMove GameState Move Move
    Returns a GameState in which the player has moved from first Move to second Move. 
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
  RETURNS: GameState
  EXAMPLES: playMove (makeGamestate insertPlayer) (3,3) (4,4) = 
                        [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".",".",".","r",".","r","."],[".",".",".","r",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]]
-}
playMove :: GameState -> Move -> Move -> GameState
playMove (x:xs) (fromrow,fromcol) (torow,tocol) = makeGamestate (removePosition (playMoveAux (concat (addPositionGameState (x:xs))) (fromrow,fromcol) (torow,tocol))) where
  {- playMoveAux ListwithPosition Move Move
      Returns a list with positions in which the player has moved from first Move to second Move. 
    PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
    RETURNS: ListwithPosition
    EXAMPLES: playMoveAux addPosition (3,3) (4,4) = 
                          [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),("r",(3,1)),(".",(3,2)),(".",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),("r",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))]
  -}
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
          | (fromrow,fromcol) == (torow-1,tocol-1) = insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))
          | (fromrow,fromcol) == (torow-1,tocol+1) = insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))
          | (fromrow,fromcol) == (torow+1,tocol+1) = insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))
          | (fromrow,fromcol) == (torow+1,tocol-1) = insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))
          | (fromrow,fromcol) == (torow-2,tocol-2) = insertAt (".",(fromrow +1,fromcol +1)) (delete (showPiece p,(fromrow +1,fromcol +1)) (insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))))
          | (fromrow,fromcol) == (torow-2,tocol+2) = insertAt (".",(fromrow +1,fromcol-1)) (delete (showPiece m,(fromrow +1,fromcol-1)) (insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))))
          | (fromrow,fromcol) == (torow+2,tocol+2) = insertAt (".",(fromrow -1,fromcol-1)) (delete (showPiece xa,(fromrow -1,fromcol-1)) (insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))))
          | (fromrow,fromcol) == (torow+2,tocol-2) = insertAt (".",(fromrow -1,fromcol+1)) (delete (showPiece a,(fromrow -1,fromcol+1)) (insertAt (q,(s,t)) (delete (r,(s,t)) (insertAt (r,(w,e)) (delete (q,(w,e)) all)))))

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
      insertatAux (r,(x,y)) [] [] _ = []
      insertatAux (r,(x,y)) ((d,(f,(g,h))):ys) ((a,(b,c)):xs) position
            | position == d = (r,(x,y)):(a,(b,c)):xs
            | x == 8 && y == 8 = (a,(b,c)):xs ++ [(r,(x,y))]
            | otherwise = [(f,(g,h))] ++ insertatAux (r,(x,y)) ys xs position

{- findPosition Position
    Finds the number indexed at that position. First element is indexed at 0, second element is indexed at 1 and so forth. 
    Otherwise is an outlier, merely there to make the code complile and should under no circumstances be used. 
  PRE: No positions bigger than (8,8), No positions smaller than (1,1).  
  RETURNS: Int
  EXAMPLES: findPosition (1,1) = 0
              findPosition (7,8) = 55
-}
findPosition :: (Eq a1, Num a1, Num a2) => (a1, a2) -> a2
findPosition (b,c)
    | b == 1 = 0 + c-1
    | b == 2 = 8 + c-1
    | b == 3 = 16 + c-1
    | b == 4 = 24 + c-1
    | b == 5 = 32 + c-1
    | b == 6 = 40 + c-1
    | b == 7 = 48 + c-1
    | b == 8 = 56 + c-1
    | otherwise = 4

{- victoryRed GameState
    Returns False if any white piece is still on the board, otherwise True. 
  PRE: Valid GameState
  RETURNS: Bool
  EXAMPLES: victoryRed [["r",","]] = True
              victoryRed [["r","w"]] = False
-}  
victoryRed :: GameState -> Bool
victoryRed (x:xs) = victoryredAux (concat (x:xs)) where
  {- victoryredAux List
      Returns False if any white piece is still on the board, otherwise True.
    VARIANT: Length of List
    PRE: True
    RETURNS: Bool
    EXAMPLES: victoryRed ["r",",","r",","] = True
                victoryRed ["r","w","r","r",",",","] = False
  -}
  victoryredAux [] = True
  victoryredAux (y:ys)
    | y == "w" || y == "W" = False
    | otherwise = victoryredAux ys

{- victoryWhite GameState
  PRE: Valid GameState
  RETURNS: False if any red pieces is still on the board, otherwise True.
  EXAMPLES: victoryWhite [["r",",","r",","]] = False
              victoryWhite [[",","w",",",",",",",","]] = True
-}        
victoryWhite :: GameState -> Bool
victoryWhite (x:xs) = victoryAux (concat (x:xs)) where
  {- victorywhiteAux List
      Returns False if any red pieces is still on the board, otherwise True. 
    PRE: Valid GameState
    RETURNS: Bool
    EXAMPLES: victoryWhite ["r",",","r",","] = False
                victoryWhite [",","w",",",",",",",","] = True
  -}
  victoryAux [] = True
  victoryAux (y:ys)
    | y == "r" || y == "R" = False
    | otherwise = victoryAux ys

{- printMove String Move Move
  Prints out what the player choose to move from and move to to the terminal for clarification.
  Post: IO () 
  SIDE EFFECTS: Prints out a string 
-}
printMove :: String -> Move -> Move -> IO ()
printMove player (row1, column1) (row2, column2) = putStrLn $ player ++ " " ++ "moves from  " ++ "(" ++ (show row1) ++ " , " ++ (show column1) ++ ")" ++ "  to  " ++ "(" ++ (show row2) ++ " , " ++ (show column2) ++ ")"

{- printboard
  Prints out what the board/gamestate looks like at the moment. 
  Post: IO () 
  SIDE EFFECTS: Prints out a board 
-}
printboard :: Show a => [[a]] -> IO ()
printboard (x:y:z:q:a:b:c:d) = do
    putStrLn $ "   1" ++ "   2" ++ "   3" ++ "   4" ++ "   5" ++ "   6" ++ "   7" ++ "   8"
    putStrLn $ "1" ++ (show x)
    putStrLn $ "2" ++ (show y)
    putStrLn $ "3" ++ (show z)
    putStrLn $ "4" ++ (show q)
    putStrLn $ "5" ++ (show a)
    putStrLn $ "6" ++ (show b)
    putStrLn $ "7" ++ (show c)
    putStrLn $ "8" ++ (show (concat d))

{- quitPlease
  Asks the user if they want to play the game again after someone wins
  Post: IO () 
  SIDE EFFECTS: Prints out a string
-}   
quitPlease :: IO ()
quitPlease = do
    putStrLn "do you want to quit? (yes/no)?"
    answer <- getLine
    when (answer == "no") main

{- checkPositionRed GameState Move Move
    Checks if last move was a jump and if so runs checkPositionsAux. If the player can jump again, returns True otherwise False. 
    PRE: Valid GameState, Moveset within (1-8,1-8) so that it's within the GameState. 
    RETURNS: Bool
    EXAMPLES: checkPositionRed (makeGamestate insertPlayer) (3,3) (4,4) = False
-}     
checkPositionRed :: GameState -> Move -> Move -> Bool
checkPositionRed (x:xs) (u,v) (w,q)
    | (u,v) == (w-2,q-2) = checkPositionsAuxRed (concat (addPositionGameState (x:xs))) (w,q)
    | (u,v) == (w+2,q+2) = checkPositionsAuxRed (concat (addPositionGameState (x:xs))) (w,q)
    | (u,v) == (w-2,q+2) = checkPositionsAuxRed (concat (addPositionGameState (x:xs))) (w,q)
    | (u,v) == (w+2,q-2) = checkPositionsAuxRed (concat (addPositionGameState (x:xs))) (w,q)
    | otherwise = False

{- checkPositionwhite GameState Move Move
    Checks if last move was a jump and if so runs checkPositionsAuxwhite. If the player can jump again, returns True otherwise False. 
    PRE: Valid GameState, Moveset within (1-8,1-8) so that it's within the GameState. 
    RETURNS: Bool
    EXAMPLES: checkPositionwhite (makeGamestate insertPlayer) (6,6) (8,8) = False
-}
checkPositionwhite :: GameState -> Move -> Move -> Bool
checkPositionwhite (x:xs) (u,v) (w,q)
    | (u,v) == (w-2,q-2) = checkPositionsAuxwhite (concat (addPositionGameState (x:xs))) (w,q)
    | (u,v) == (w+2,q+2) = checkPositionsAuxwhite (concat (addPositionGameState (x:xs))) (w,q)
    | (u,v) == (w-2,q+2) = checkPositionsAuxwhite (concat (addPositionGameState (x:xs))) (w,q)
    | (u,v) == (w+2,q-2) = checkPositionsAuxwhite (concat (addPositionGameState (x:xs))) (w,q)
    | otherwise = False

{- checkPositionsAuxRed ListwithPosition Move
    Checks the positions around the Move and returns True if a jump can be made from that position otherwise False. 
    PRE: Moveset within (1-8,1-8) so that it's within the board. 
    RETURNS: Bool
    EXAMPLES: checkPositionsAuxRed addPosition (6,6) = False
-}
checkPositionsAuxRed ::ListwithPosition -> Move -> Bool
checkPositionsAuxRed ys (w,q) =
  let (z,(x,j)) = ys !! (findPosition (w,q)) in
  let (p,(l,e)) = ys !! (findPosition (w+1,q+1)) in
  let (m,(i,o)) = ys !! (findPosition (w+1,q-1)) in
  let (a,(b,n)) = ys !! (findPosition (w-1,q+1)) in
  let (s,(f,g)) = ys !! (findPosition (w-1,q-1)) in
  let (dt,(ro,co)) = ys !! (findPosition (w+2,q+2)) in
  let (dy,(row,col)) = ys !! (findPosition (w+2,q-2)) in
  let (dx,(ry,cy)) = ys !! (findPosition (w-2,q+2)) in
  let (dq,(ru,cu)) = ys !! (findPosition (w-2,q-2)) in
  checkPARaux (z,(x,j)) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu)) where
    {- checkRed (String,Position) (String,Position) (String,Position) (String,Position) (String,Position) (String,Position) (String,Position) (String,Position) (String,Position)
        Checks the positions around the first (String,Position) and returns True if a jump can be made from that position otherwise False. 
        PRE: Moveset within (1-8,1-8) so that it's within the board. 
        RETURNS: Bool
        EXAMPLE: checkRed ("r",(3,3)) (".",(4,4)) (".",(4,2)) ("r",(2,4)) ("r",(2,2)) (".",(5,5)) (".",(5,1)) ("r",(1,5)) ("r",(1,1)) = False
                 checkRed ("r",(3,3)) ("w",(4,4)) (".",(4,2)) ("r",(2,4)) ("r",(2,2)) (".",(5,5)) (".",(5,1)) ("r",(1,5)) ("r",(1,1)) = True
    -}
    checkPARaux:: (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> Bool
    checkPARaux (z,(x,j)) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
      | (x == 1 || x == 2) && (j == 1 || j == 2) = if ((p == "w" || p == "W") && dt == ".") then True else False
      | (x == 1 || x == 2) && (j == 7 || j == 8) = if ((m == "w" || m == "W") && dy == ".") then True else False
      | (x == 7 || x == 8) && (j == 1 || j == 2) = if ((a == "w" || a == "W") && dx == ".") then True else False
      | (x == 7 || x == 8) && (j == 7 || j == 8) = if ((s == "w" || s == "W") && dq == ".") then True else False
      | (x == 1 || x == 2) && (j >= 3 && j <= 6) = if ((p == "w" || p == "W") && dt == ".") || ((m == "w" || m == "W") && dy == ".") then True else False
      | (x >= 3 && x <= 6) && (j == 1 || j == 2) = if ((a == "w" || a == "W") && dx == ".") || ((p == "w" || p == "W") && dt == ".") then True else False
      | (x == 7 || x == 8) && (j >=3 && j <= 6) = if ((s == "w" || s == "W") && dq == ".") || ((a == "w" || a == "W") && dx == ".") then True else False
      | (x >= 3 && x <= 6) && (j == 7 || j == 8) = if ((s == "w" || s == "W") && dq == ".") || ((m == "w" || m == "W") && dy == ".") then True else False
      | otherwise = if ((p == "w" || p == "W") && dt == ".") || ((m == "w" || m == "W") && dy == ".") || ((a == "w" || a == "W") && dx == ".") || ((s == "w" || s == "W") && dq == ".") then True else False

{- checkPositionsAuxwhite ListwithPosition Move
    Checks the positions around the Move and returns True if a jump can be made from that position otherwise False. 
    PRE: Moveset within (1-8,1-8) so that it's within the board. 
    RETURNS: Bool
    EXAMPLES: checkPositionsAuxwhite [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),
                                    (".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),
                                    ("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),
                                    (".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),
                                    (".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),("r",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),
                                    (".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),
                                    ("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),
                                    (".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))] (6,4) = True
-}
checkPositionsAuxwhite ::ListwithPosition -> Move -> Bool
checkPositionsAuxwhite ys (w,q) =
  let (z,(x,j)) = ys !! (findPosition (w,q)) in
  let (p,(l,e)) = ys !! (findPosition (w+1,q+1)) in
  let (m,(i,o)) = ys !! (findPosition (w+1,q-1)) in
  let (a,(b,n)) = ys !! (findPosition (w-1,q+1)) in
  let (s,(f,g)) = ys !! (findPosition (w-1,q-1)) in
  let (dt,(ro,co)) = ys !! (findPosition (w+2,q+2)) in
  let (dy,(row,col)) = ys !! (findPosition (w+2,q-2)) in
  let (dx,(ry,cy)) = ys !! (findPosition (w-2,q+2)) in
  let (dq,(ru,cu)) = ys !! (findPosition (w-2,q-2)) in
  checkPAWaux (z,(x,j)) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu)) where
    {- checkWhite (String,Position) (String,Position) (String,Position) (String,Position) (String,Position) (String,Position) (String,Position) (String,Position) (String,Position)
        Checks the positions around the first (String,Position) and returns True if a jump can be made from that position otherwise False. 
        PRE: Moveset within (1-8,1-8) so that it's within the board. 
        RETURNS: Bool
        EXAMPLE: checkWhite ("w",(6,2)) ("w",(7,3)) ("w",(7,1)) (".",(5,3)) (".",(5,1)) ("w",(8,4)) (".",(7,8)) (".",(4,4)) (".",(3,8)) = False
    -}
    checkPAWaux::(String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> (String,Position) -> Bool
    checkPAWaux (z,(x,j)) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
      | (x == 1 || x == 2) && (j == 1 || j == 2) = if ((p == "r" || p == "R") && dt == ".") then True else False
      | (x == 1 || x == 2) && (j == 7 || j == 8) = if ((m == "r" || m == "R") && dy == ".") then True else False
      | (x == 7 || x == 8) && (j == 1 || j == 2) = if ((a == "r" || a == "R") && dx == ".") then True else False
      | (x == 7 || x == 8) && (j == 7 || j == 8) = if ((s == "r" || s == "R") && dq == ".") then True else False
      | (x == 1 || x == 2) && (j >= 3 && j <= 6) = if ((p == "r" || p == "R")  && dt == ".") || ((m == "r" || m == "R") && dy == ".") then True else False
      | (x >= 3 && x <= 6) && (j == 1 || j == 2) = if ((a == "r" || a == "R") && dx == ".") || ((p == "r" || p == "R") && dt == ".") then True else False
      | (x == 7 || x == 8) && (j >=3 && j <= 6) = if ((s == "r" || s == "r") && dq == ".") || ((a == "r" || a == "R") && dx == ".") then True else False
      | (x >= 3 && x <= 6) && (j == 7 || j == 8) = if ((s == "r" || s == "R") && dq == ".") || ((m == "r" || m == "R") && dy == ".") then True else False
      | otherwise = if ((p == "r" || p == "R") && dt == ".") || ((m == "r" || m == "R") && dy == ".") || ((a == "r" || a == "R") && dx == ".") || ((s == "r" || s == "R") && dq == ".") then True else False


--------------------------------------------------------------------------------
-- Test Cases
--------------------------------------------------------------------------------

--showPiece
test1 = TestCase $ assertEqual "showPiece"
            ("R") ((showPiece "R"))

-- upgradePiece

test2 = TestCase $ assertEqual "upgradePiece"
            "W" (upgradePiece "w")

-- removePosition
test3 = TestCase $ assertEqual "removePosition"
            (removePosition addPosition) (insertPlayer)

--addPositionGameState
test4 = TestCase $ assertEqual "addPositionGameState"
            ([[("r",(1,1))]]) (addPositionGameState [["r"]])
            
test5 = TestCase $ assertEqual "addPositionGameState"
            (makeGamestate addPosition) (addPositionGameState [insertPlayer])
            
test6 = TestCase $ assertEqual "addPositionGameState"
            ([[("r",(1,1)),(".",(1,2)),("r",(1,3))]]) (addPositionGameState [["r",".","r"]])

test7 = TestCase $ assertEqual "insertPlayerWhite"
            ([]) (insertPlayerWhite [])
            
test8 = TestCase $ assertEqual "insertPlayerWhite"
            (["w","."]) (insertPlayerWhite ["r","."])
            
test9 = TestCase $ assertEqual "makeGamestate"
            ([["w","."]]) (makeGamestate (insertPlayerWhite ["r","."]))
            
test10 = TestCase $ assertEqual "makeGamestate"
            (addPositionGameState (makeGamestate insertPlayer)) (makeGamestate addPosition)

test11 = TestCase $ assertEqual "checkifcanUpgradeRed"
            (True) (checkifcanUpgradeRed (8,8))

test12 = TestCase $ assertEqual "checkifcanUpgradeRed"
            (False) (checkifcanUpgradeRed (5,5))
            
test13 = TestCase $ assertEqual "checkifcanUpgradeWhite"
            (True) (checkifcanUpgradeWhite (1,4))
            
test14 = TestCase $ assertEqual "upgradeRed"
            (newinput) (upgradeRed input (8,8)) where
                input = [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","r"]]
                newinput = [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","R"]]
                
test15 = TestCase $ assertEqual "upgradeRed"
            (input) (upgradeRed input (1,1)) where
                input = [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]]
                
test16 = TestCase $ assertEqual "upgradeWhite"
            (newinput) (upgradeWhite input (1,1)) where
                input = [["w",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]]
                newinput = [["W",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]]
                
test17 = TestCase $ assertEqual "upgradeWhite"
            (input) (upgradeWhite input (8,6)) where
                input = [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]]
                
test18 = TestCase $ assertEqual "validMoveRed"
            (True) (validMoveRed (makeGamestate insertPlayer) (3,3) (4,4))
  
test19 = TestCase $ assertEqual "validMoveRed"
            (False) (validMoveRed (makeGamestate insertPlayer) (3,3) (6,6))

test20 = TestCase $ assertEqual "validMoveRedAux"
            (False) (validMoveRedAux addPosition (3,3) (6,6))            

test21 = TestCase $ assertEqual "validMoveRedAux"
            (True) (validMoveRedAux addPosition (3,1) (4,2))
            
test22 = TestCase $ assertEqual "validMoveRedAuxAux"
            (True) (validMoveRedAuxAux addPosition (3,3) (4,4) (".",(4,4)) (".",(4,2)) ("r",(2,4)) ("r",(2,2)) (".",(5,5)) (".",(5,1)) ("r",(1,5)) ("r",(1,1)))
            
test23 = TestCase $ assertEqual "validMoveRedAuxAux"
            (False) (validMoveRedAuxAux addPosition (3,3) (5,5) (".",(4,4)) (".",(4,2)) ("r",(2,4)) ("r",(2,2)) (".",(5,5)) (".",(5,1)) ("r",(1,5)) ("r",(1,1)))


test24 = TestCase $ assertEqual "validMoveRedJump"
            (False) (validMoveRedJump addPosition (3,3) (5,5) (".",(4,4)) (".",(4,2)) ("r",(2,4)) ("r",(2,2)) (".",(5,5)) (".",(5,1)) ("r",(1,5)) ("r",(1,1)))
            
test25 = TestCase $ assertEqual "validMoveWhite"
            (True) (validMoveWhite (makeGamestate insertPlayer) (6,6) (5,5))

test26 = TestCase $ assertEqual "validMoveWhite"
            (validMoveauxWhite addPosition (6,6) (5,5)) (validMoveWhite (makeGamestate insertPlayer) (6,6) (5,5))

test27 = TestCase $ assertEqual "validMoveWhiteAuxAux"
            (True) (validMoveWhiteAuxAux addPosition (6,2) (5,3) ("w",(7,3)) ("w",(7,1)) (".",(5,3)) (".",(5,1)) ("w",(8,4)) (".",(7,8)) (".",(4,4)) (".",(3,8)))

test28 = TestCase $ assertEqual "validMoveAll"
            (True) (validMoveAll addPosition (3,3) (4,4))

test29 = TestCase $ assertEqual "validMoveAll"
            (False) (validMoveAll addPosition (3,3) (8,8))

test30 = TestCase $ assertEqual "validMoveAll"
            (True) (validMoveAll addPosition (3,1) (4,2))
            
test31 = TestCase $ assertEqual "validMoveAll"
            (False) (validMoveWhiteJump addPosition (6,2) (4,4) ("w",(7,3)) ("w",(7,1)) (".",(5,3)) (".",(5,1)) ("w",(8,4)) (".",(7,8)) (".",(4,4)) (".",(3,8)))

test32 = TestCase $ assertEqual "validchoiceRed"
            (True) (validchoiceRed (makeGamestate insertPlayer) (3,1)) 
     
test33 = TestCase $ assertEqual "validchoiceRed"
            (False) (validchoiceRed (makeGamestate insertPlayer) (4,7)) 

test34 = TestCase $ assertEqual "validchoiceRed"
            (False) (validchoiceRed (makeGamestate insertPlayer) (4,7))
            
test35 = TestCase $ assertEqual "validchoiceWhite"
            (False) (validchoiceWhite (makeGamestate insertPlayer) (1,3))

test36 = TestCase $ assertEqual "validchoiceWhite"
            (True) (validchoiceWhite (makeGamestate insertPlayer) (8,8))

test37 = TestCase $ assertEqual "validPlaceRedaux"
            (True) (validPlaceRedaux addPosition (1,1))
            
test38 = TestCase $ assertEqual "validPlaceRedaux"
            (validchoiceRed (makeGamestate insertPlayer) (1,1)) (validPlaceRedaux addPosition (1,1))

test39 = TestCase $ assertEqual "validPlaceWhiteaux"
            (True) (validPlaceWhiteaux addPosition (7,7))

test40 = TestCase $ assertEqual "validPlaceWhiteaux"
            (False) (validPlaceWhiteaux addPosition (3,3))

test41 = TestCase $ assertEqual "playMove"
            (input) (playMove (makeGamestate insertPlayer) (3,3) (4,4)) where
                input = [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".",".",".","r",".","r","."],[".",".",".","r",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]]

test42 = TestCase $ assertEqual "playMove"
            (input) (playMove (makeGamestate insertPlayer) (3,3) (4,2)) where
                input = [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".",".",".","r",".","r","."],[".","r",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]]                

                
test43 = TestCase $ assertEqual "insertAt"
            (input) (insertAt ("r",(0,0)) [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(0,1)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8))]) where
                input = [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(0,1)),("r",(0,1)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8))]
                
test44 = TestCase $ assertEqual "insertAt"
            (input) (insertAt ("r",(1,3)) [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(0,1)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8))]) where
                input = [("r",(1,1)),(".",(1,2)),("r",(1,3)),("r",(1,3)),(".",(1,4)),("r",(0,1)),("r",(0,1)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8))]
            
test45 = TestCase $ assertEqual "findPosition"
            (1) (findPosition (1,2))
            
test46 = TestCase $ assertEqual "findPosition"
            (56) (findPosition (8,1))

test47 = TestCase $ assertEqual "victoryRed"
            (True) (victoryRed [["r","r"]])
            
test48 = TestCase $ assertEqual "victoryRed"
            (False) (victoryRed [["r",".","r",".","r",".","r","w"]])

test49 = TestCase $ assertEqual "victoryWhite"
            (False) (victoryWhite [["r",".","w","w","w",".","w","w"]])
            
test50 = TestCase $ assertEqual "victoryWhite"
            (True) (victoryWhite [[".",".",".",".",".",".",".","."]])
            
test51 = TestCase $ assertEqual "checkPositionRed"
            (False) (checkPositionRed (makeGamestate insertPlayer) (3,3) (4,4))

test52 = TestCase $ assertEqual "checkPositionwhite"
            (False) (checkPositionwhite (makeGamestate insertPlayer) (6,6) (8,7))
            
test53 = TestCase $ assertEqual "checkPositionRed"
            (False) (checkPositionwhite (makeGamestate insertPlayer) (6,6) (8,7))
                 
test54 = TestCase $ assertEqual "checkPositionAuxRed"
            (False) (checkPositionsAuxRed addPosition (5,5))
            
test55 = TestCase $ assertEqual "checkPositionsAuxwhite"
            (True) (checkPositionsAuxRed input (6,4)) where
                input = [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),
                    (".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),
                        ("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),
                            (".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),
                                    (".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),("r",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),
                                    (".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),
                                    ("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),
                                    (".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))]



-- for running all the tests
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9,
    test10, test11, test12, test13, test14, test15, test16, test17, test18, test19, test20, test21, test22, test23, test24, test25, test26, test27, test28, test29, test30, test31, test32, test33
    test34, test35, test36, test37, test38, test39, test40, test41, test42, test43, test44, test45, test46, test47, test48, test49, test50, test51, test52, test53, test54, test55]
