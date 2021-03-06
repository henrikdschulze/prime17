import Data.Char
import Control.Monad
import Control.Exception
import Debug.Trace
import Data.List
import Test.HUnit

{- the Player is represented by a color and a type
  INVARIANT:
    There are only two colors and two types.
    Each Color corresponds to a character and uppercase or lowercase to represent the character's type.
    Player Red Normal & Player Red King represent Player Red
    while Player White Normal & Player White King represent Player White.
-}
data Player = Player Color Type

{- the Color is either Red or White 
  INVARIANT:
    There are only two colors and each Color corresponds to a character. "r" for Red and "w" for White.
    These will represent the colors of the pieces one the board. Color must be represented as a String.
-}
data Color = Red | White 

{- the Type is either Normal or King 
  INVARIANT:
    There are only two types. Lowercase characters represent "Normal" pieces and uppercase characters
    represent "King" pieces, meaning for example that player Red have managed to move one of the pieces
    to the last row. "r" for Red Normal, "R" for Red King and "w" for White Normal and "W" for White King.
-}
data Type = Normal | King 

{- GameState is the board and is represented as a list of strings in a list.
  INVARIANT:
    The GameState is never to be empty and should always be 8x8 in size. No numbers should be in the GameState.
-}
type GameState = [[String]]

{- GameStateWithPositionsMatrix is the board with positions, where the position is represented as a
    tuple with the first number being the row number, and the second number being the column number.
  INVARIANT:
    The GameState is never to be empty and should always be 8x8 in size. The positions cannot be Floats.
-}
type GameStateWithPositionsMatrix = [[(String,Position)]]

{- GameStateWithPositionsList is the a concatenation of the board with positions.
    This version is needed to find specific elements in the list.
  INVARIANT:
    The GameStateWithPositionsList should always have the same number of elements as the GameState.
    The positions cannot be Floats.
-}
type GameStateWithPositionsList = [(String,Position)]

{- List is a list of strings. It's basically the board but with every element in one row instead.
  INVARIANT:
    Only Strings, also the list should never be empty.
-}
type List = [String]

{- The Position is represented as a tuple with the first number being the row number and the second
    number being the column number.
  INVARIANT:
    row number are 1-8. Column numbers are 1-8. No numbers outside these are valid.
-}
type Position = (Int, Int)

{- readPiece Player
    Converts the Player data type to a String.
  PRE: -
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
    what string it is in some position in the game state.
  PRE: -
  RETURNS: String
  SIDE EFFECTS: -
  EXAMPLES:
    showPiece "w"
      == "w"
    showPiece "r"
      == "r"
-}
showPiece :: String -> String
showPiece "w" = readPiece (Player White Normal)
showPiece "W" = readPiece (Player White King)
showPiece "r" = readPiece (Player Red Normal)
showPiece "R" = readPiece (Player Red King)
showPiece "." = readPiece (Player None NotHere)

{- emptyBoard
    Makes an "empty board" with 64 elements. It's filled with "." to signify that there are
    no pieces at any position.
  PRE: -
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

{- removePosition gameStateWithPositionsList
    Removes the positions from the list, returning only a list with the empty tiles and pieces.
  VARIANT: Length of gameStateWithPositionsList
  PRE: -
  RETURNS: List
  SIDE EFFECTS: -
  EXAMPLES:
    removePosition []
      == []
    removePosition [("r",(1,1)),(".",(1,2))]
      == ["r","."]
-}
removePosition :: GameStateWithPositionsList -> List
removePosition [] = []
removePosition xs = map fst xs

{- addPositionsToGameState GameState
    Adds positions to the GameState. Row number is the first number and goes from 1 to 8.
    Column number is the second number and goes from 1 to 8.
  PRE: No empty list
  RETURNS: GameStateWithPositionsMatrix
  SIDE EFFECTS: -
  EXAMPLES:
    addPositionsToGameState [["r","."],["r"]]
      == [[("r",(1,1)),(".",(1,2)),("r",(1,3))]]
-}
addPositionsToGameState :: GameState -> GameStateWithPositionsMatrix
addPositionsToGameState (x:xs) = makeGamestate (addPositionsToGameStateAux (concat(x:xs)) (1,1))
{- addPositionsToGameStateAux listPlain position
    Adds positions to the elements in the list. Row number is the first number and goes from 1 to 8.
    Column number is the second number and goes from 1 to 8.
  VARIANT: Length of listPlain
  PRE: -
  RETURNS: GameStateWithPositionsList
  SIDE EFFECTS: -
  EXAMPLES:
    addPositionsToGameStateAux [] (1,2)
      == []
    addPositionsToGameStateAux ["r","."] (1,1)
      == [("r",(1,1)),(".",(1,2))]
-}
addPositionsToGameStateAux:: List -> Position -> GameStateWithPositionsList
addPositionsToGameStateAux [] _ = []
addPositionsToGameStateAux (x:xs) (a,b)
  | b < 8 = [(x,(a,b))] ++ addPositionsToGameStateAux xs (a,b+1)
  | b == 8 = [(x,(a,b))] ++ addPositionsToGameStateAux xs (a+1,b-7)

{- insertPlayers
    Inserts the player pieces into the empty list.
  PRE: -
  RETURNS: List
  SIDE EFFECTS: -
  EXAMPLES:
    insertPlayers
  == ["r",".","r",".","r",".","r",".",
      ".","r",".","r",".","r",".","r",
      "r",".","r",".","r",".","r",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".","w",".","w",".","w",".","w",
      "w",".","w",".","w",".","w",".",
      ".","w",".","w",".","w",".","w"]
-}
insertPlayers::List
insertPlayers = let (x:y:xs) = emptyBoard in
  insertPlayersWhite $ insertPlayersRed (x:y:xs) readPiece (Player Red Normal) (1)
{- insertPlayersRed List readPiece Player Int
    Inserts the red player pieces into the empty list as strings.
  PRE: List must have an even number of elements
  RETURNS: List
  SIDE EFFECTS: -
  EXAMPLES:
    insertPlayersRed emptyBoard readPiece (Player Red Normal) 1
  == ["r",".","r",".","r",".","r",".",
      ".","r",".","r",".","r",".","r",
      "r",".","r",".","r",".","r",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".","."]
-}
insertPlayersRed :: List -> (Player -> String) -> Player -> Int -> List
insertPlayersRed [] _ _ _ = []
insertPlayersRed (x:y:xs) readPiece (Player Red Normal) (num)
  | num <= 4 = (readPiece (Player Red Normal)):y:(insertPlayersRed xs readPiece (Player Red Normal) (num+1))
  | num > 4 && num <= 8 = y:(readPiece (Player Red Normal)):(insertPlayersRed xs readPiece (Player Red Normal) (num+1))
  | num > 8 && num < 12 = (readPiece (Player Red Normal)):y:(insertPlayersRed xs readPiece (Player Red Normal) (num+1))
  | num == 12 = (readPiece (Player Red Normal)):y:xs

{- insertPlayersWhite List
    Insert the player White pieces into a list.
  PRE: List must have an even number of elements
  RETURNS: List
  SIDE EFFECTS: -
  EXAMPLES:
    insertPlayersWhite ["r",".","r"]
      *** Exception: checkers.hs:(203,5)-(208,59): Non-exhaustive patterns in function insertWhitePlayersAux
    insertPlayersWhite emptyBoard
  == [".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".","w",".","w",".","w",".","w",
      "w",".","w",".","w",".","w",".",
      ".","w",".","w",".","w",".","w"]
-}
insertPlayersWhite:: List -> List
insertPlayersWhite xs = reverse(insertWhitePlayersAux (reverse xs) readPiece (Player White Normal) 1)
{- insertWhitePlayersAux List readPiece Player Int
    Insert the player White pieces into the empty list as strings.
  PRE: List must have an even number of elements
  RETURNS: List
  SIDE EFFECTS: -
  EXAMPLES:
    insertWhitePlayersAux emptyBoard readPiece (Player White Normal) 1
  == ["w",".","w",".","w",".","w",".",
      ".","w",".","w",".","w",".","w",
      "w",".","w",".","w",".","w",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".",".",
      ".",".",".",".",".",".",".","."]
-}
insertWhitePlayersAux :: List -> (Player -> String) -> Player -> Int -> List
insertWhitePlayersAux [] _ _ _ = []
insertWhitePlayersAux (x:y:ys) readPiece (Player White Normal) (num)
  | num <= 4            = (readPiece (Player White Normal)):y:
      (insertWhitePlayersAux ys readPiece (Player White Normal) (num+1))
  | num > 4 && num <= 8 = y:(readPiece (Player White Normal)):
      (insertWhitePlayersAux ys readPiece (Player White Normal) (num+1))
  | num > 8 && num < 12 = (readPiece (Player White Normal)):y:
      (insertWhitePlayersAux ys readPiece (Player White Normal) (num+1))
  | num == 12           = (readPiece (Player White Normal)):y:ys

{- makeGamestate listOrig
    Makes a list into a list of sublists, where each sublist has length 8.
  PRE: listOrig must not be empty.
  RETURNS: [[a]]
  SIDE EFFECTS: -
  EXAMPLES:
    makeGamestate ["r",".",".",".",".",".",".",".","."]
      == [["r",".",".",".",".",".",".","."],["."]]
-}
makeGamestate:: [a] -> [[a]]
makeGamestate l = rows l
{- rows list
    Makes a list into a list of lists
  VARIANT: Length of list
  PRE: True
  RETURNS: [[a]]
  EXAMPLES:
    rows ["r",".",".",".",".",".",".",".","."]
      == [["r",".",".",".",".",".",".","."],["."]]
-}
rows:: [a] -> [[a]]
rows [] = []
rows l = [take 8 l] ++ rows (drop 8 l)

{- genGameState
    Generates the starting game state.
  PRE: None
  RETURNS: IO GameState
  SIDE EFFECTS: -
  EXAMPLES: -
-}
genGameState:: IO GameState
genGameState = do
  return (makeGamestate (insertPlayers))

{- main
    Starts the game and generates the game state.
  PRE: None
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
    Prints out the current game state, generates new gamestates from the moves from both players,
  and finally checks victory conditions
  PRE: Valid gameState
  RETURNS: IO ()
  SIDE EFFECTS: Prints out the gameState on the screen.
                If victory is achieved then prints out a message about who won.
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

{- checkIfCanMakeKingRed position
    Checks if the current position is on row 8, making it valid for the piece to become a red King.
  PRE: -
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES:
    checkIfCanMakeKingRed (8,1)
      == True
    checkIfCanMakeKingRed (5,5)
      == False
-}
checkIfCanMakeKingRed :: Position -> Bool
checkIfCanMakeKingRed (a,b)
    | a == 8 = True
    | otherwise = False

 {- checkIfCanMakeKingWhite position
    Checks if the current position is on row 1, making it valid for the piece to become a white King.
  PRE: -
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES:
    checkIfCanMakeKingWhite (1,1)
      == True
    checkIfCanMakeKingWhite (4,5)
      == False
-}
checkIfCanMakeKingWhite :: Position -> Bool
checkIfCanMakeKingWhite (a,b)
    | a == 1 = True
    | otherwise = False

{- makeKingRed gameState position
    Upgrades the piece to king if it is a valid transformation.
  PRE: gameState must not be an empty list and the position must be valid. The list must be
          large enough to include the position checked for because of the index operation (!!).
  RETURNS: GameState
  SIDE EFFECTS: -
  EXAMPLES:
    makeKingRed [["r",".","r",".","r",".","r","."],
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
makeKingRed :: GameState -> Position -> GameState
makeKingRed newGameState (a,b)
 = makeGamestate (removePosition (makeKingAuxRed (concat (addPositionsToGameState newGameState)) (a,b)))
{- makeKingAuxRed gameStateWithPositionsList position
    Upgrades the piece to king if it is a valid transformation.
  PRE: gameStateWithPositionsList must not be an empty list and the position must be valid. The list must
          be large enough to include the position checked for because of the index operation (!!).
  RETURNS: GameStateWithPositionsList
  SIDE EFFECTS: -
  EXAMPLES:
    makeKingAuxRed
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
makeKingAuxRed :: GameStateWithPositionsList -> Position -> GameStateWithPositionsList
makeKingAuxRed ((y,(c,d)):ys) (a,b) =
    let (r,(s,t)) = ((y,(c,d)):ys) !! (findPosition (a,b))
    in makeKingAuxAuxRed ((y,(c,d)):ys) (r,(s,t))
{- makeKingAuxAuxRed GameStateWithPositionsList (String,Position)
    Upgrades the piece to king if it is a valid transformation.
  PRE: the first list must not be an empty list and the position (a,b) must be valid.
  RETURNS: GameStateWithPositionsList
  SIDE EFFECTS: -
  EXAMPLES:
    makeKingAuxAuxRed
    [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),
     (".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),
     ("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),
     (".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),
     (".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),
     (".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),
     ("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),
     (".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("r",(8,8))] ("r",(8,8))
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
makeKingAuxAuxRed:: GameStateWithPositionsList -> (String,Position) -> GameStateWithPositionsList
makeKingAuxAuxRed all@((y,(c,d)):ys) (r,(s,t))
  | s == 8 = insertAt ("R",(s,t)) (delete (r,(s,t)) all)
  | otherwise = ((y,(c,d)):ys)

{- makeKingWhite gameState position
    Upgrades the piece to king if it is a valid transformation.
  PRE: gameState must not be an empty list and the position must be valid. The list must be
          large enough to include the position checked for because of the index operation (!!).
  RETURNS: GameState
  SIDE EFFECTS: -
  EXAMPLES:
    makeKingWhite [["w",".","r",".","r",".","r","."]] (1,1)
      == [["W",".","r",".","r",".","r","."]]
-}
makeKingWhite :: GameState -> Position -> GameState
makeKingWhite newGameState (a,b)
 = makeGamestate (removePosition (makeKingAuxWhite (concat (addPositionsToGameState newGameState)) (a,b)))
{- makeKingAuxWhite gameStateWithPositionsList position
    Upgrades the piece to king if it is a valid transformation.
  PRE: gameStateWithPositionsList must not be an empty list and the position must be valid. The list must
          be large enough to include the position checked for because of the index operation (!!).
  RETURNS: GameStateWithPositionsList
  SIDE EFFECTS: -
  EXAMPLES:
    makeKingAuxWhite [("w",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4))] (1,1)
      == [("W",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4))]
-}
makeKingAuxWhite :: GameStateWithPositionsList -> Position -> GameStateWithPositionsList
makeKingAuxWhite ((y,(c,d)):ys) (a,b) =
    let (r,(s,t)) = ((y,(c,d)):ys) !! (findPosition (a,b))
    in makeKingAuxAuxWhite ((y,(c,d)):ys) (r,(s,t))
{- makeKingAuxAuxWhite GameStateWithPositionsList (String,Position)
    Upgrades the piece to king if it is a valid transformation.
  PRE: the first list must not be an empty list and the position (a,b) must be valid.
  RETURNS: GameStateWithPositionsList
  SIDE EFFECTS: -
  EXAMPLES:
    makeKingAuxAuxWhite [("w",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4))] ("w",(1,1))
      == [("W",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4))]
-}
makeKingAuxAuxWhite:: GameStateWithPositionsList -> (String,Position) -> GameStateWithPositionsList
makeKingAuxAuxWhite all@((y,(c,d)):ys) (r,(s,t))
  | s == 1 = insertAt ("W",(s,t)) (delete (r,(s,t)) all)
  | otherwise = ((y,(c,d)):ys)

{- playerMoveRed gamestate
    From the current game state, gets input from the player for one position the player want to move
    from and one position to move to and prints these to the screen. Prints out the current game state
    for the player to see. Also checks if certain conditions are met and if so, does those functions.
  PRE: Valid gameState
  RETURNS: IO GameState
  SIDE EFFECTS: Prints out the current game state and messages depending on some conditions.
  EXAMPLES: -
-}
playerMoveRed :: GameState -> IO GameState
playerMoveRed gameState = do
  putStrLn "Player Red move from"
  move1 <- readMove
  putStrLn "Player Red move to"
  move2 <- readMove
  printMove "Player Red" move1 move2
  if (validPlaceRed gameState move1 && validMoveRed gameState move1 move2) then do
    newGameState <- return (playMove gameState move1 move2)
    if checkIfCanMakeKingRed move2 then do
       newsGameState <- return (makeKingRed newGameState move2)
       printboard newsGameState
       return $ newsGameState
    else do
      printboard newGameState
      if checkPositionRed newGameState move1 move2 then do
         doubleMoveRed newGameState move2 else
         return $ playMove gameState move1 move2
      else do
        putStrLn "Invalid Position. You can only move your own pieces and move diagonally"
        playerMoveRed gameState

{- doubleMoveRed gameState position
    If after a jump, the player is able to jump again, this function forces the player to move
    the same piece again.
  PRE: Valid gameState and valid Position
  RETURNS: IO GameState
  SIDE EFFECTS: Prints out the current game state and messages depending on some conditions.
  EXAMPLES: -
-}
doubleMoveRed:: GameState -> Position -> IO GameState
doubleMoveRed newGameState (a,b) = do
    let move3 = (a,b)
    putStrLn ("Player Red have to move from  " ++ (show a) ++ " , " ++ (show b))
    putStrLn "Player Red move to"
    move4 <- readMove
    printMove "Player Red" move3 move4
    if (validPlaceRed newGameState move3 && validMoveRed newGameState move3 move4) then do
    newnewGameState <- return (playMove newGameState move3 move4)
    if checkIfCanMakeKingRed move4 then do
      newsGameState <- return (makeKingRed newnewGameState move4)
      printboard newsGameState
      return $ newsGameState
    else do
      printboard newnewGameState
      if checkPositionRed newnewGameState move3 move4 then do
        doubleMoveRed newnewGameState move4 else return $ playMove newGameState move3 move4
      else do
        putStrLn "Invalid Position. You can only move your own pieces and move diagonally"
        doubleMoveRed newGameState (a,b)

{- playerMoveWhite gamestate
    From the current game state, gets input from the player for one position the player want to move
    from and one position to move to and prints these to the screen. Prints out the current game state
    for the player to see. Also checks if certain conditions are met and if so, does those functions.
  PRE: Valid gameState
  RETURNS: IO GameState
  SIDE EFFECTS: Prints out the current game state and messages depending on some conditions.
  EXAMPLES: -
-}
playerMoveWhite :: GameState -> IO GameState
playerMoveWhite gameState = do
  putStrLn "Player White move from"
  move1 <- readMove
  putStrLn "Player White move to"
  move2 <- readMove
  printMove "Player White" move1 move2
  if (validplaceWhite gameState move1 && validMoveWhite gameState move1 move2) then do
    newGameState <- return (playMove gameState move1 move2)
    if checkIfCanMakeKingWhite move2 then do
       newsGameState <- return (makeKingWhite newGameState move2)
       printboard newsGameState
       return $ newsGameState
    else do
      if checkPositionwhite newGameState move1 move2 then do
        printboard newGameState
        doubleMoveWhite newGameState move2 else do
            printboard newGameState
            return $ playMove gameState move1 move2
      else do
         putStrLn "Invalid Position. You can only move your own pieces and move diagonally"
         playerMoveWhite gameState

{- doubleMoveWhite gameState position
    If after a jump, the player is able to jump again, this function forces the player to move
    the same piece again.
  PRE: Valid gameState and valid Position
  RETURNS: IO GameState
  SIDE EFFECTS: Prints out the current game state and messages depending on some conditions.
  EXAMPLES: -
-}
doubleMoveWhite:: GameState -> Position -> IO GameState
doubleMoveWhite newGameState (a,b) = do
    let move3 = (a,b)
    putStrLn ("Player White have to move from  " ++ (show a) ++ " , " ++ (show b))
    putStrLn "Player White move to"
    move4 <- readMove
    printMove "Player White" move3 move4
    if (validplaceWhite newGameState move3 && validMoveWhite newGameState move3 move4) then do
      newnewGameState <- return (playMove newGameState move3 move4)
      printboard newnewGameState
      if checkIfCanMakeKingWhite move4 then do
        newsGameState <- return (makeKingWhite newnewGameState move4)
        printboard newsGameState
        return $ newsGameState
      else do
        if checkPositionwhite newnewGameState move3 move4 then do
          doubleMoveWhite newnewGameState move4 else return $ playMove newGameState move3 move4
        else do
          putStrLn "Invalid Position. You can only move your own pieces and move diagonally"
          doubleMoveWhite newGameState (a,b)

{- readMove
  Reads a move from standard input
  Post: A move object
  SIDE EFFECTS: Reads one or more lines from standard input
-}
readMove :: (IO Position) -- reads input from
readMove = do
  catch (do
    line <- getLine
    evaluate (read line))  -- evaluate required to force conversion of line to position
    ((\_ -> do  -- exception handler
    putStrLn "Invalid input. Correct format: (row,column)"
    readMove) :: SomeException -> IO Position)

{- validMoveRed gameState positionFrom positionTo
    Returns True if the move is valid, returns false otherwise.
  PRE: No empty gamestate
  RETURNS: True if the move is valid, False otherwise.
  SIDE EFFECTS: -
  EXAMPLES:
    validMoveRed [insertPlayers] (3,3) (4,4)
      == True
    validMoveRed [insertPlayers] (3,1) (5,2)
      == False
-}
validMoveRed :: GameState -> Position -> Position -> Bool
validMoveRed (x:xs) (u,v) (w,q) = validMoveAuxRed (concat (addPositionsToGameState (x:xs))) (u,v) (w,q)
{- validMoveAuxRed gameStateWithPositionsList positionFrom positionTo
    Returns True if the move is valid, returns false otherwise.
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board.
  RETURNS: True if the move is valid, False otherwise.
  SIDE EFFECTS: -
  EXAMPLES:
    validMoveAuxRed (concat $ addPositionsToGameState (makeGamestate insertPlayers)) (3,3) (4,4)
  == True
    validMoveAuxRed (concat $ addPositionsToGameState (makeGamestate insertPlayers)) (3,3) (5,5)
  == False
-}
validMoveAuxRed ::GameStateWithPositionsList -> Position -> Position -> Bool
validMoveAuxRed [] _ _ = False
validMoveAuxRed ys (u,v) (w,q) =
  let (p,(l,e)) = ys !! (findPosition (u+1,v+1)) in
  let (m,(i,o)) = ys !! (findPosition (u+1,v-1)) in
  let (a,(b,n)) = ys !! (findPosition (u-1,v+1)) in
  let (s,(f,g)) = ys !! (findPosition (u-1,v-1)) in
  let (dt,(ro,co)) = ys !! (findPosition (u+2,v+2)) in
  let (dy,(row,col)) = ys !! (findPosition (u+2,v-2)) in
  let (dx,(ry,cy)) = ys !! (findPosition (u-2,v+2)) in
  let (dq,(ru,cu)) = ys !! (findPosition (u-2,v-2)) in
  validMoveAuxAuxRed ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g))
    (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))

{- validMoveAuxAuxRed gameStateWithPositionsList positionFrom positionTo (String, Position) (String, Position)
(String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position)
    Checks the move to see if it is a jump or a step. If a jump it runs the function validJumpRed.
    Otherwise runs validMoveOneStep. All (String,Position) correspond to various positions and strings
    around the first Position. The positions in order are, if we consider that the 'from'-position
    is in the center:     LowerRight, LowerLeft, UpperRight, Upperleft,
    LowerRightLowerRight, LowerLeftLowerLeft, UpperRightUpperRight, UpperLeftUpperLeft.
    Note that this function should recieve positionFrom and all (string,positions) from the board.
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board.
  RETURNS: True if the move is valid, False otherwise.
  SIDE EFFECTS: -
  EXAMPLES:
    validMoveAuxAuxRed (concat $ addPositionsToGameState (makeGamestate insertPlayers)) (3,3) (4,4)
      (".",(4,4)) (".",(4,2)) ("r",(2,4)) ("r",(2,2)) (".",(5,5)) (".",(5,1)) ("r",(1,5)) ("r",(1,1))
        == True
-}
validMoveAuxAuxRed:: GameStateWithPositionsList -> Position -> Position -> (String, Position) -> (String,Position)
                      -> (String,Position) -> (String,Position) -> (String,Position)
                      -> (String,Position) -> (String,Position) -> (String, Position) -> Bool
validMoveAuxAuxRed ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g))
 (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
  | (u,v) == (w-2,q-2) = validJumpRed ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g))
      (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
  | (u,v) == (w-2,q+2) = validJumpRed ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g))
      (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
  | (u,v) == (w+2,q+2) = validJumpRed ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g))
      (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
  | (u,v) == (w+2,q-2) = validJumpRed ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g))
      (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
  | otherwise          = validMoveOneStep ys (u,v) (w,q)

{- validJumpRed gameStateWithPositionsList positionFrom positionTo (String, Position) (String, Position)
(String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position)
    Returns True if the move is valid, returns false otherwise. This function considers a valid jump,
    that is to say two steps diagonally from the starting position (first move argument).
    All (String,Position) corresponds to various positions and strings around the first Position.
    The positions in order are, if we consider that the 'from'-position is in the center:
    LowerRight, LowerLeft, UpperRight, Upperleft,
    LowerRightLowerRight, LowerLeftLowerLeft, UpperRightUpperRight, UpperLeftUpperLeft.
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board.
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES:
    validJumpRed (concat $ addPositionsToGameState (makeGamestate insertPlayers)) (3,3) (5,5)
      (".",(4,4)) (".",(4,2)) ("r",(2,4)) ("r",(2,2)) (".",(5,5)) (".",(5,1)) ("r",(1,5)) ("r",(1,1))
        == False
-}
validJumpRed:: GameStateWithPositionsList -> Position -> Position -> (String, Position) -> (String,Position)
                    -> (String,Position) -> (String,Position) -> (String,Position)
                    -> (String,Position) -> (String,Position) -> (String, Position) -> Bool
validJumpRed ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g))
 (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
  | (u == 1 || u == 2) && (v == 1 || v == 2) = if ((p == "w" || p == "W") && dt == ".") then True else False
  | (u == 1 || u == 2) && (v == 7 || v == 8) = if ((m == "w" || m == "W") && dy == ".") then True else False
  | (u == 7 || u == 8) && (v == 1 || v == 2) = if ((a == "w" || a == "W") && dx == ".") then True else False
  | (u == 7 || u == 8) && (v == 7 || v == 8) = if ((s == "w" || s == "W") && dq == ".") then True else False
  | (u == 1 || u == 2) && (v >= 3 && v <= 6) = if ((p == "w" || p == "W") && dt == ".") ||
      ((m == "w" || m == "W") && dy == ".") then True else False
  | (u >= 3 && u <= 6) && (v == 1 || v == 2) = if ((a == "w" || a == "W") && dx == ".") ||
      ((p == "w" || p == "W") && dt == ".") then True else False
  | (u == 7 || u == 8) && (v >=3  && v <= 6) = if ((s == "w" || s == "W") && dq == ".") ||
      ((a == "w" || a == "W") && dx == ".") then True else False
  | (u >= 3 && u <= 6) && (v == 7 || v == 8) = if ((s == "w" || s == "W") && dq == ".") ||
      ((m == "w" || m == "W") && dy == ".") then True else False
  | otherwise = if ((p == "w" || p == "W") && dt == ".") || ((m == "w" || m == "W") && dy == ".") || 
    ((a == "w" || a == "W") && dx == ".") || ((s == "w" || s == "W") && dq == ".") then True else False

{- validMoveWhite gameState positionFrom positionTo
    Returns True if the move is valid for Player White, returns false otherwise.
  PRE: No empty gamestate
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES:
    validMoveWhite[insertPlayers] (6,6) (5,5)
      == True
    validMoveWhite[insertPlayers] (6,6) (5,6)
      == False
-}
validMoveWhite :: GameState -> Position -> Position -> Bool
validMoveWhite (x:xs) (u,v) (w,q) = validMoveAuxWhite (concat (addPositionsToGameState (x:xs))) (u,v) (w,q)
{- validMoveAuxWhite gameStateWithPositionsList positionFrom positionTo
    Returns True if the move is valid for Player White, returns false 
    move is
    the position the piece moves from. Second move it the position the player wants to move to.
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board.
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES:
    validMoveAuxWhite (concat $ addPositionsToGameState (makeGamestate insertPlayers)) (7,7) (8,8)
      == False
    validMoveAuxWhite (concat $ addPositionsToGameState (makeGamestate insertPlayers)) (6,6) (5,5)
      == True
-}
validMoveAuxWhite ::GameStateWithPositionsList -> Position -> Position -> Bool
validMoveAuxWhite [] _ _ = False
validMoveAuxWhite ys (u,v) (w,q) =
  let (p,(l,e)) = ys !! (findPosition (u+1,v+1)) in
  let (m,(i,o)) = ys !! (findPosition (u+1,v-1)) in
  let (a,(b,n)) = ys !! (findPosition (u-1,v+1)) in
  let (s,(f,g)) = ys !! (findPosition (u-1,v-1)) in
  let (dt,(ro,co)) = ys !! (findPosition (u+2,v+2)) in
  let (dy,(row,col)) = ys !! (findPosition (u+2,v-2)) in
  let (dx,(ry,cy)) = ys !! (findPosition (u-2,v+2)) in
  let (dq,(ru,cu)) = ys !! (findPosition (u-2,v-2)) in
  validMoveAuxAuxWhite ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g))
    (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
{- validMoveAuxAuxWhite gameStateWithPositionsList positionFrom positionTo (String, Position) (String, Position)
(String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position)
    Checks the move to see if it is a jump or a step. If a jump it runs the function validJumpWhite.
    Otherwise runs validMoveOneStep. All (String,Position) correspond to various positions and strings
    around the first Position. The positions in order are, if we consider that the 'from'-position
    is in the center:    LowerRight, LowerLeft, UpperRight, Upperleft,
    LowerRightLowerRight, LowerLeftLowerLeft, UpperRightUpperRight, UpperLeftUpperLeft.
    Note that this function should recieve positionFrom and all (string,positions) from the board.
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board.
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES:
    validMoveAuxAuxWhite (concat $ addPositionsToGameState (makeGamestate insertPlayers)) (6,2) (5,3)
      ("w",(7,3)) ("w",(7,1)) (".",(5,3)) (".",(5,1)) ("w",(8,4)) (".",(7,8)) (".",(4,4)) (".",(3,8))
        == True
-}
validMoveAuxAuxWhite:: GameStateWithPositionsList -> Position -> Position -> (String, Position) -> (String,Position)
                        -> (String,Position) -> (String,Position) -> (String,Position)
                        -> (String,Position) -> (String,Position) -> (String, Position) -> Bool
validMoveAuxAuxWhite ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g))
 (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
   | (u,v) == (w-2,q-2) = validJumpWhite ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g))
      (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
   | (u,v) == (w-2,q+2) = validJumpWhite ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g))
      (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
   | (u,v) == (w+2,q+2) = validJumpWhite ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g))
      (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
   | (u,v) == (w+2,q-2) = validJumpWhite ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g))
      (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
   | otherwise = validMoveOneStep ys (u,v) (w,q)

{- validMoveOneStep gameStateWithPositionsList positionFrom positionTo
    Returns true if you have a valid move were you move your piece one step diagonally
    from the starting position (first move). Works for both Red and White.
  VARIANT: Length of gameStateWithPositionsList
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board.
  RETURNS: True if the move is valid.
  SIDE EFFECTS: -
  EXAMPLES:
    validMoveOneStep(concat $ addPositionsToGameState (makeGamestate insertPlayers)) (3,3) (4,4)
      == True
    validMoveOneStep(concat $ addPositionsToGameState (makeGamestate insertPlayers)) (3,3) (8,8)
      == False
-}
validMoveOneStep :: GameStateWithPositionsList -> Position -> Position -> Bool
validMoveOneStep [] _ _ = False
validMoveOneStep ((y,(c,d)):ys) (u,v) (w,q)
    | (u,v) == (w-1,q-1) = if (c,d) == (w,q) && y == "." then True else validMoveOneStep ys (u,v) (w,q)
    | (u,v) == (w-1,q+1) = if (c,d) == (w,q) && y == "." then True else validMoveOneStep ys (u,v) (w,q)
    | (u,v) == (w+1,q+1) = if (c,d) == (w,q) && y == "." then True else validMoveOneStep ys (u,v) (w,q)
    | (u,v) == (w+1,q-1) = if (c,d) == (w,q) && y == "." then True else validMoveOneStep ys (u,v) (w,q)
    | otherwise = False

{- validJumpWhite gameStateWithPositionsList positionFrom positionTo (String, Position) (String, Position)
 (String, Position) (String, Position) (String, Position) (String, Position) (String, Position) (String, Position)
    Returns True if the move is valid, returns false otherwise. This function considers a valid jump,
    that is to say two steps diagonally from the starting position (first move argument).
    All (String,Position) corresponds to various positions and strings around the first Position.
    The positions in order are, if we consider that the 'from'-position is in the center:
    LowerRight, LowerLeft, UpperRight, Upperleft,
    LowerRightLowerRight, LowerLeftLowerLeft, UpperRightUpperRight, UpperLeftUpperLeft.
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board.
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES:
    validMoveAuxAuxWhite (concat $ addPositionsToGameState (makeGamestate insertPlayers)) (6,2) (4,4)
      ("w",(7,3)) ("w",(7,1)) (".",(5,3)) (".",(5,1)) ("w",(8,4)) (".",(7,8)) (".",(4,4)) (".",(3,8))
        == False
-}
validJumpWhite:: GameStateWithPositionsList -> Position -> Position -> (String, Position) -> (String,Position)
                  -> (String,Position) -> (String,Position) -> (String,Position)
                  -> (String,Position) -> (String,Position) -> (String, Position) -> Bool
validJumpWhite ys (u,v) (w,q) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g))
 (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
    | (u == 1 || u == 2) && (v == 1 || v == 2) = if ((p == "r" || p == "R") && dt == ".") then True else False
    | (u == 1 || u == 2) && (v == 7 || v == 8) = if ((m == "r" || m == "R") && dy == ".") then True else False
    | (u == 7 || u == 8) && (v == 1 || v == 2) = if ((a == "r" || a == "R") && dx == ".") then True else False
    | (u == 7 || u == 8) && (v == 7 || v == 8) = if ((s == "r" || s == "R") && dq == ".") then True else False
    | (u == 1 || u == 2) && (v >= 3 && v <= 6) = if ((p == "r" || p == "R") && dt == ".") ||
        ((m == "r" || m == "R") && dy == ".") then True else False
    | (u >= 3 && u <= 6) && (v == 1 || v == 2) = if ((a == "r" || a == "R") && dx == ".") ||
        ((p == "r" || p == "R") && dt == ".") then True else False
    | (u == 7 || u == 8) && (v >= 3 && v <= 6) = if ((s == "r" || s == "R") && dq == ".") ||
        ((a == "r" || a == "R") && dx == ".") then True else False
    | (u >= 3 && u <= 6) && (v == 7 || v == 8) = if ((s == "r" || s == "R") && dq == ".") ||
        ((m == "r" || m == "R") && dy == ".") then True else False
    | otherwise = if ((p == "r" || p == "R") && dt == ".") || ((m == "r" || m == "R") &&
        dy == ".") || ((a == "r" || a == "R") && dx == ".") || ((s == "r" || s == "R") && dq == ".")
          then True else False

{- validPlaceRed gameState position
    Returns true if position contains a Red piece. This means that the player Red may only move Red
    pieces. Position in this case is the position you want to move from. Note that this function only
    cares that you select one correct piece, it does not care if you can move from there or not.
  VARIANT: Length of list
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board.
  RETURNS: true if the current piece has color Red.
  SIDE EFFECTS: -
  EXAMPLES:
    validPlaceRed [insertPlayers] (3,3)
      == True
    validPlaceRed [insertPlayers] (4,4)
      == False
-}
validPlaceRed :: GameState -> Position -> Bool
validPlaceRed (x:xs) (s,t) = validPlaceAuxRed (concat (addPositionsToGameState (x:xs))) (s,t)
{- validPlaceAuxRed gameStateWithPositionsList position
    Returns true if position contains a Red piece. This means that the player Red may only move red pieces.
    Position in this case is the position you want to move from. It checks which position you want to move
    from and checks which piece is at that position. If the piece is "r" or "R" then True. Else False.
    Note that this function only cares that you select one correct piece, it does not care if you can
    move from there or not.
    VARIANT: Length of list
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board.
  RETURNS: True if there is a Red piece at position.
  SIDE EFFECTS: -
  EXAMPLES:
    validPlaceAuxRed (concat $ addPositionsToGameState (makeGamestate insertPlayers)) (1,1)
      == True
    validPlaceAuxRed (concat $ addPositionsToGameState (makeGamestate insertPlayers)) (4,4)
      == False
-}
validPlaceAuxRed :: GameStateWithPositionsList -> Position -> Bool
validPlaceAuxRed [] _ = False
validPlaceAuxRed ((y,(c,d)):ys) (s,t)
  | c == s && d == t = if (y == "r" || y == "R") then True else False
  | otherwise = validPlaceAuxRed ys (s,t)

{- validplaceWhite gameState position
    Returns true if position contains a White piece. This means that the player White may only move White
    pieces. Position in this case is the position you want to move from. Note that this function only
    cares that you select one correct piece, it does not care if you can move from there or not.
  VARIANT: -
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
  RETURNS: Bool
  SIDE EFFECTS: -
  EXAMPLES:
    validplaceWhite [insertPlayers] (5,5)
      == False
    validplaceWhite [insertPlayers] (8,8)
      == True
-}  
validplaceWhite :: GameState -> Position -> Bool
validplaceWhite (x:xs) (s,t) = validPlaceAuxWhite (concat (addPositionsToGameState (x:xs))) (s,t)
{- validPlaceAuxWhite gameStateWithPositionsList position
    Returns true if position contains a White piece. This means that the player White may only move White pieces.
    Position in this case is the position you want to move from. It checks which position you want to move
    from and checks which piece is at that position. If the piece is "w" or "W" then True. Else False.
    Note that this function only cares that you select one correct piece, it does not care if you can
    move from there or not.
  VARIANT: Length of list
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
  RETURNS: Bool
  EXAMPLES:
    validPlaceAuxWhite (concat $ addPositionsToGameState (makeGamestate insertPlayers)) (7,7)
      == True
-}
validPlaceAuxWhite :: GameStateWithPositionsList -> Position -> Bool
validPlaceAuxWhite [] _ = False
validPlaceAuxWhite ((y,(c,d)):ys) (s,t)
  | c == s && d == t = if (y == "w" || y == "W") then True else False
  | otherwise = validPlaceAuxWhite ys (s,t)

{- playMove gameState positionFrom positionTo
    Returns a GameState in which the player has moved from first Position to second Position. 
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
  RETURNS: GameState
  EXAMPLES:
    playMove [insertPlayers] (3,3) (4,4)
     == 
              [["r",".","r",".","r",".","r","."],
               [".","r",".","r",".","r",".","r"],
               ["r",".",".",".","r",".","r","."],
               [".",".",".","r",".",".",".","."],
               [".",".",".",".",".",".",".","."],
               [".","w",".","w",".","w",".","w"],
               ["w",".","w",".","w",".","w","."],
               [".","w",".","w",".","w",".","w"]]
-}
playMove :: GameState -> Position -> Position -> GameState
playMove (x:xs) (fromrow,fromcol) (torow,tocol) = makeGamestate $ removePosition $ playMoveAux
 (concat (addPositionsToGameState (x:xs))) (fromrow,fromcol) (torow,tocol)
{- playMoveAux gameStateWithPositionsList positionFrom positionTo
    Returns a list with positions in which the player has moved from first Position to second Position. 
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
  RETURNS: GameStateWithPositionsList
  EXAMPLES:
    playMoveAux (concat $ addPositionsToGameState (makeGamestate insertPlayers)) (3,3) (4,4)
  ==
  [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),
   (".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),
   ("r",(3,1)),(".",(3,2)),(".",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),
   (".",(4,1)),(".",(4,2)),(".",(4,3)),("r",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),
   (".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),
   (".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),
   ("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),
   (".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))]
-}
playMoveAux [] _ _ = []
playMoveAux all@(x:xs) (fromrow,fromcol) (torow,tocol) = let (r,(s,t)) = all !! (findPosition (fromrow,fromcol)) in
  let (q,(w,e)) = all !! (findPosition (torow,tocol)) in
  let (p,(l,ef)) = all !! (findPosition (fromrow+1,fromcol+1)) in
  let (m,(i,o)) = all !! (findPosition (fromrow+1,fromcol-1)) in
  let (a,(b,n)) = all !! (findPosition (fromrow-1,fromcol+1)) in
  let (xa,(f,g)) = all !! (findPosition (fromrow-1,fromcol-1)) in
    makeaMove (x:xs) (r,(s,t)) (q,(w,e)) (fromrow,fromcol) (torow,tocol) (p,(l,ef)) (m,(i,o)) (a,(b,n)) (xa,(f,g))
{- makeaMove GameStateWithPositionsList (String,Position) (String,Position) Position Position (String,Position) (String,Position) (String,Position) (String,Position)
    Returns a list with positions. All (String,Position) corresponds to various positions and strings around the first Position, except the first one which is the piece corresponding to the first Position.
    The positions in order are, if we consider that first Position are in the center, LowerRight, LowerLeft, UpperRight,Upperleft. It deletes the (String,Position) you want to move to and inserts the piece
    at the same position. It then deletes the piece from the starting position and inserts a "." since it's now empty.  
  PRE: Valid move (row 1-8, column 1-8), no numbers outside the board. 
  RETURNS: GameStateWithPositionsList
  EXAMPLE: makeaMove [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),
                        (".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),
                        ("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),
                        (".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),
                        (".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),
                        (".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),
                        ("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),
                        (".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))] 
                        ("r",(3,3)) (".",(4,4)) (3,3) (4,4) (".",(4,4)) (".",(4,2)) ("r",(2,4)) ("r",(2,2)) =
                        
                        [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),
                        (".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),
                        ("r",(3,1)),(".",(3,2)),(".",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),
                        (".",(4,1)),(".",(4,2)),(".",(4,3)),("r",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),
                        (".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),
                        (".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),
                        ("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),
                        (".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))]
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

{- insertAt:: (String,Position) GameStateWithPositionsList 
    Inserts a (String,Position) at the correct position in a list with the first element being position 0, second one being position 1 and so forth. 
  PRE: No positions bigger than (8,8), No positions smaller than (1,1)
  RETURNS: GameStateWithPositionsList
  EXAMPLES:
    insertAt ("r",(0,1)) [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(0,1)),("r",(1,5))]
  == [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(0,1)),("r",(0,1)),("r",(1,5))]
              insertAt ("r",(1,2)) [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(0,1)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8))] = 
              [("r",(1,1)),("r",(1,2)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(0,1)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8))]
-}
insertAt:: (String,Position) -> GameStateWithPositionsList -> GameStateWithPositionsList
insertAt (r,(x,y)) [] = []
insertAt (r,(x,y)) ((a,(b,c)):xs) = let ((d,(f,(g,h))):ys) = zip [0..] ((a,(b,c)):xs) in
  let position = (findPosition (x,y)) in
    insertatAux (r,(x,y)) ((d,(f,(g,h))):ys) ((a,(b,c)):xs) position
{- insertatAux:: (String,Position) [(Int, (String,Position))] -> GameStateWithPositionsList -> Int 
    Inserts a (String,Position) at the correct position in a list with the first element being position 0, second one being position 1 and so forth.
    It does this by comparing the two Ints provided in the arguments, if they are the same it inserts the (String,Position) at that place in the list. 
  PRE: No positions bigger than (8,8), No positions smaller than (1,1). The last Int provided must be the one you get from inserting (x,y) into findPosition.  
  RETURNS: GameStateWithPositionsList
  EXAMPLES: insertatAux ("r",(1,2)) [(0,("r",(1,1))),(1,(".",(1,2))),(2,("r",(1,3))),(3,(".",(1,4))),(4,("r",(1,5))),(5,(".",(1,6))),(6,("r",(1,7))),(7,(".",(1,8)))]
              [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8))] 1 = 
              [("r",(1,1)),("r",(1,2)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8))]
-}
insertatAux (r,(x,y)) [] [] _ = []
insertatAux (r,(x,y)) ((d,(f,(g,h))):ys) ((a,(b,c)):xs) position
      | position == d = (r,(x,y)):(a,(b,c)):xs
      | x == 8 && y == 8 = (a,(b,c)):xs ++ [(r,(x,y))]
      | otherwise = [(f,(g,h))] ++ insertatAux (r,(x,y)) ys xs position

{- findPosition position
    Finds the number indexed at that position. First element is indexed at 0, second element is indexed at 1 and so forth. 
    Otherwise is an outlier, merely there to make the code complile and should under no circumstances be used. 
  PRE: No positions bigger than (8,8), No positions smaller than (1,1).  
  RETURNS: Int
  EXAMPLES: findPosition (1,1) = 0
              findPosition (7,8) = 55
-}
findPosition :: Position -> Int
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
victoryRed (x:xs) = victoryAuxRed (concat (x:xs))
{- victoryAuxRed List
    Returns False if any white piece is still on the board, otherwise True.
  VARIANT: Length of List
  PRE: True
  RETURNS: Bool
  EXAMPLES: victoryRed ["r",",","r",","] = True
              victoryRed ["r","w","r","r",",",","] = False
-}
victoryAuxRed [] = True
victoryAuxRed (y:ys)
  | y == "w" || y == "W" = False
  | otherwise = victoryAuxRed ys

{- victoryWhite GameState
  PRE: Valid GameState
  RETURNS: False if any red pieces is still on the board, otherwise True.
  EXAMPLES: victoryWhite [["r",",","r",","]] = False
              victoryWhite [[",","w",",",",",",",","]] = True
-}        
victoryWhite :: GameState -> Bool
victoryWhite (x:xs) = victoryAuxWhite (concat (x:xs))
{- victorywhiteAux List
    Returns False if any red pieces is still on the board, otherwise True. 
  PRE: Valid GameState
  RETURNS: Bool
  EXAMPLES: victoryWhite ["r",",","r",","] = False
              victoryWhite [",","w",",",",",",",","] = True
-}
victoryAuxWhite [] = True
victoryAuxWhite (y:ys)
  | y == "r" || y == "R" = False
  | otherwise = victoryAuxWhite ys

{- printMove String Position Position
  Prints out what the player choose to move from and move to to the terminal for clarification.
  Post: IO () 
  SIDE EFFECTS: Prints out a string 
-}
printMove :: String -> Position -> Position -> IO ()
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
    when (answer == "yes") main

{- checkPositionRed gameState positionFrom positionTo
    Checks if last move was a jump and if so runs checkPositionsAux. If the player can jump again, returns True otherwise False. 
    PRE: Valid GameState, Moveset within (1-8,1-8) so that it's within the GameState. 
    RETURNS: Bool
    EXAMPLES: checkPositionRed [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]] (3,3) (4,4) = False
-}     
checkPositionRed :: GameState -> Position -> Position -> Bool
checkPositionRed (x:xs) (u,v) (w,q)
    | (u,v) == (w-2,q-2) = checkPositionsAux (concat (addPositionsToGameState (x:xs))) (w,q)
    | (u,v) == (w+2,q+2) = checkPositionsAux (concat (addPositionsToGameState (x:xs))) (w,q)
    | (u,v) == (w-2,q+2) = checkPositionsAux (concat (addPositionsToGameState (x:xs))) (w,q)
    | (u,v) == (w+2,q-2) = checkPositionsAux (concat (addPositionsToGameState (x:xs))) (w,q)
    | otherwise = False

{- checkPositionwhite gameState positionFrom positionTo
    Checks if last move was a jump and if so runs checkPositionsAuxwhite. If the player can jump again, returns True otherwise False. 
    PRE: Valid GameState, Moveset within (1-8,1-8) so that it's within the GameState. 
    RETURNS: Bool
    EXAMPLES: checkPositionwhite [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]] (6,6) (8,8) = False
-}
checkPositionwhite :: GameState -> Position -> Position -> Bool
checkPositionwhite (x:xs) (u,v) (w,q)
    | (u,v) == (w-2,q-2) = checkPositionsAuxwhite (concat (addPositionsToGameState (x:xs))) (w,q)
    | (u,v) == (w+2,q+2) = checkPositionsAuxwhite (concat (addPositionsToGameState (x:xs))) (w,q)
    | (u,v) == (w-2,q+2) = checkPositionsAuxwhite (concat (addPositionsToGameState (x:xs))) (w,q)
    | (u,v) == (w+2,q-2) = checkPositionsAuxwhite (concat (addPositionsToGameState (x:xs))) (w,q)
    | otherwise = False

{- checkPositionsAux gameStateWithPositionsList position
    Checks the positions around the Position and returns True if a jump can be made from that position otherwise False. 
    PRE: Moveset within (1-8,1-8) so that it's within the board. 
    RETURNS: Bool
    EXAMPLES: checkPositionsAux [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8)),(".",(2,1)),("r",(2,2)),(".",(2,3)),("r",(2,4)),(".",(2,5)),("r",(2,6)),(".",(2,7)),("r",(2,8)),("r",(3,1)),(".",(3,2)),("r",(3,3)),(".",(3,4)),("r",(3,5)),(".",(3,6)),("r",(3,7)),(".",(3,8)),(".",(4,1)),(".",(4,2)),(".",(4,3)),(".",(4,4)),(".",(4,5)),(".",(4,6)),(".",(4,7)),(".",(4,8)),(".",(5,1)),(".",(5,2)),(".",(5,3)),(".",(5,4)),(".",(5,5)),(".",(5,6)),(".",(5,7)),(".",(5,8)),(".",(6,1)),("w",(6,2)),(".",(6,3)),("w",(6,4)),(".",(6,5)),("w",(6,6)),(".",(6,7)),("w",(6,8)),("w",(7,1)),(".",(7,2)),("w",(7,3)),(".",(7,4)),("w",(7,5)),(".",(7,6)),("w",(7,7)),(".",(7,8)),(".",(8,1)),("w",(8,2)),(".",(8,3)),("w",(8,4)),(".",(8,5)),("w",(8,6)),(".",(8,7)),("w",(8,8))] (6,6) = False
-}
checkPositionsAux ::GameStateWithPositionsList -> Position -> Bool
checkPositionsAux ys (w,q) =
  let (z,(x,j)) = ys !! (findPosition (w,q)) in
  let (p,(l,e)) = ys !! (findPosition (w+1,q+1)) in
  let (m,(i,o)) = ys !! (findPosition (w+1,q-1)) in
  let (a,(b,n)) = ys !! (findPosition (w-1,q+1)) in
  let (s,(f,g)) = ys !! (findPosition (w-1,q-1)) in
  let (dt,(ro,co)) = ys !! (findPosition (w+2,q+2)) in
  let (dy,(row,col)) = ys !! (findPosition (w+2,q-2)) in
  let (dx,(ry,cy)) = ys !! (findPosition (w-2,q+2)) in
  let (dq,(ru,cu)) = ys !! (findPosition (w-2,q-2)) in
  checkPARaux (z,(x,j)) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
{- checkRed (String,Position) (String,Position) (String,Position) (String,Position) (String,Position) (String,Position) (String,Position) (String,Position) (String,Position)
    Checks the positions around the first (String,Position) and returns True if a jump can be made from that position otherwise False. 
    PRE: Moveset within (1-8,1-8) so that it's within the board. 
    RETURNS: Bool
    EXAMPLE: checkRed ("r",(3,3)) (".",(4,4)) (".",(4,2)) ("r",(2,4)) ("r",(2,2)) (".",(5,5)) (".",(5,1)) ("r",(1,5)) ("r",(1,1)) = False
             checkRed ("r",(3,3)) ("w",(4,4)) (".",(4,2)) ("r",(2,4)) ("r",(2,2)) (".",(5,5)) (".",(5,1)) ("r",(1,5)) ("r",(1,1)) = True
-}
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

checkPositionsAuxwhite ::GameStateWithPositionsList -> Position -> Bool
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
  checkPAWaux (z,(x,j)) (p,(l,e)) (m,(i,o)) (a,(b,n)) (s,(f,g)) (dt,(ro,co)) (dy,(row,col)) (dx,(ry,cy)) (dq,(ru,cu))
{- checkWhite (String,Position) (String,Position) (String,Position) (String,Position) (String,Position) (String,Position) (String,Position) (String,Position) (String,Position)
    Checks the positions around the first (String,Position) and returns True if a jump can be made from that position otherwise False. 
    PRE: Moveset within (1-8,1-8) so that it's within the board. 
    RETURNS: Bool
    EXAMPLE:
    checkWhite ("w",(6,2)) ("w",(7,3)) ("w",(7,1)) (".",(5,3)) (".",(5,1)) ("w",(8,4)) (".",(7,8)) (".",(4,4)) (".",(3,8))
      == False
-}
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
  
  
  

test0 = TestCase $ assertEqual "showPiece"
            ("R") ((showPiece "R"))

test1 = TestCase $ assertEqual "removePosition"
            (removePosition (concat(addPositionsToGameState [insertPlayers]))) (insertPlayers)

test2 = TestCase $ assertEqual "addPositionToGameState"
            ([[("r",(1,1))]]) (addPositionsToGameState [["r"]])
 
 
test3 = TestCase $ assertEqual "addPositionToGameState"
            ([[("r",(1,1)),(".",(1,2)),("r",(1,3))]]) (addPositionsToGameState [["r",".","r"]])

test4 = TestCase $ assertEqual "insertPlayersWhite"
            ([]) (insertPlayersWhite [])
            
test5 = TestCase $ assertEqual "insertPlayersWhite"
            (["r","w"]) (insertPlayersWhite ["r","."])
            
test6 = TestCase $ assertEqual "makeGamestate"
            ([["r","w"]]) (makeGamestate (insertPlayersWhite ["r","."]))
            

test7 = TestCase $ assertEqual "checkIfCanMakeKingRed"
            (True) (checkIfCanMakeKingRed (8,8))

test8 = TestCase $ assertEqual "checkIfCanMakeKingRed"
            (False) (checkIfCanMakeKingRed (5,5))
            
test9 = TestCase $ assertEqual "checkIfCanMakeKingWhite"
            (True) (checkIfCanMakeKingWhite (1,4))
            
test10 = TestCase $ assertEqual "makeKingRed"
            (newinput) (makeKingRed input (8,8)) where
                input = [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","r"]]
                newinput = [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","R"]]
                
test11 = TestCase $ assertEqual "makeKingRed"
            (input) (makeKingRed input (1,1)) where
                input = [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]]
                
test12 = TestCase $ assertEqual "makeKingWhite"
            (newinput) (makeKingWhite input (1,1)) where
                input = [["w",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]]
                newinput = [["W",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]]
                
test13 = TestCase $ assertEqual "makeKingWhite"
            (input) (makeKingWhite input (8,6)) where
                input = [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".","r",".","r",".","r","."],[".",".",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]]
                
test14 = TestCase $ assertEqual "validMoveRed"
            (True) (validMoveRed (makeGamestate insertPlayers) (3,3) (4,4))
  
test15 = TestCase $ assertEqual "validMoveRed"
            (False) (validMoveRed (makeGamestate insertPlayers) (3,3) (6,6))



test16 = TestCase $ assertEqual "validJumpRed"
            (False) (validJumpRed (concat(addPositionsToGameState [insertPlayers])) (3,3) (5,5) (".",(4,4)) (".",(4,2)) ("r",(2,4)) ("r",(2,2)) (".",(5,5)) (".",(5,1)) ("r",(1,5)) ("r",(1,1)))
            
test17 = TestCase $ assertEqual "validMoveWhite"
            (True) (validMoveWhite (makeGamestate insertPlayers) (6,6) (5,5))

 
test18 = TestCase $ assertEqual "validMoveOneStep"
            (True) (validMoveOneStep (concat (addPositionsToGameState [insertPlayers])) (3,3) (4,4))

test19 = TestCase $ assertEqual "validMoveOneStep"
            (False) (validMoveOneStep (concat(addPositionsToGameState [insertPlayers])) (3,3) (8,8))

test20 = TestCase $ assertEqual "validMoveOneStep"
            (True) (validMoveOneStep (concat(addPositionsToGameState [insertPlayers])) (3,1) (4,2))
            
test21 = TestCase $ assertEqual "validJumpWhite"
            (False) (validJumpWhite (concat(addPositionsToGameState [insertPlayers])) (6,2) (4,4) ("w",(7,3)) ("w",(7,1)) (".",(5,3)) (".",(5,1)) ("w",(8,4)) (".",(7,8)) (".",(4,4)) (".",(3,8)))

test22 = TestCase $ assertEqual "validPlaceRed"
            (True) (validPlaceRed (makeGamestate insertPlayers) (3,1)) 
      
test23 = TestCase $ assertEqual "validPlaceRed"
            (False) (validPlaceRed (makeGamestate insertPlayers) (4,7)) 

            
test24 = TestCase $ assertEqual "validplaceWhite"
            (False) (validplaceWhite (makeGamestate insertPlayers) (1,3))

test25 = TestCase $ assertEqual "validplaceWhite"
            (True) (validplaceWhite (makeGamestate insertPlayers) (8,8))


test26 = TestCase $ assertEqual "playMove"
            (input) (playMove (makeGamestate insertPlayers) (3,3) (4,4)) where
                input = [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".",".",".","r",".","r","."],[".",".",".","r",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]]

test27 = TestCase $ assertEqual "playMove"
            (input) (playMove (makeGamestate insertPlayers) (3,3) (4,2)) where
                input = [["r",".","r",".","r",".","r","."],[".","r",".","r",".","r",".","r"],["r",".",".",".","r",".","r","."],[".","r",".",".",".",".",".","."],[".",".",".",".",".",".",".","."],[".","w",".","w",".","w",".","w"],["w",".","w",".","w",".","w","."],[".","w",".","w",".","w",".","w"]]                

                
test28 = TestCase $ assertEqual "insertAt"
            (input) (insertAt ("r",(0,0)) [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8))]) where
                input = [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(0,0)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8))]
                
test29 = TestCase $ assertEqual "insertAt"
            (input) (insertAt ("r",(1,3)) [("r",(1,1)),(".",(1,2)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8))]) where
                input = [("r",(1,1)),(".",(1,2)),("r",(1,3)),("r",(1,3)),(".",(1,4)),("r",(1,5)),(".",(1,6)),("r",(1,7)),(".",(1,8))]
            
test30 = TestCase $ assertEqual "findPosition"
            (1) (findPosition (1,2))
            
test31 = TestCase $ assertEqual "findPosition"
            (56) (findPosition (8,1))

test32 = TestCase $ assertEqual "victoryRed"
            (True) (victoryRed [["r","r"]])
            
test33 = TestCase $ assertEqual "victoryRed"
            (False) (victoryRed [["r",".","r",".","r",".","r","w"]])

test34 = TestCase $ assertEqual "victoryWhite"
            (False) (victoryWhite [["r",".","w","w","w",".","w","w"]])
            
test35 = TestCase $ assertEqual "victoryWhite"
            (True) (victoryWhite [[".",".",".",".",".",".",".","."]])
            
test36 = TestCase $ assertEqual "checkPositionRed"
            (False) (checkPositionRed (makeGamestate insertPlayers) (3,3) (4,4))

test37 = TestCase $ assertEqual "checkPositionwhite"
            (False) (checkPositionwhite (makeGamestate insertPlayers) (6,6) (8,7))
            
test38 = TestCase $ assertEqual "checkPositionRed"
            (False) (checkPositionwhite (makeGamestate insertPlayers) (6,6) (8,7))
--                             


runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9,
    test10, test11, test12, test13, test14, test15, test16, test17, test18, test19, test20, test21, test22, test23, test24,
        test25, test26, test27, test28, test29, test30, test31, test32, test33,
    test34, test35, test36, test37, test38]
-- for running all the tests
