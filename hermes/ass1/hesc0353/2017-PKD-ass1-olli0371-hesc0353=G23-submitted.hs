{-
 * Uppsala University - 2017 fall
 * Program Design and Data Structures, 1DL201
 * Assignment 1
 * Copyright 2017 Ola Lindqvist & Henke Alfken(Schulze), Group G 23
 *
 * Inspired by:
  https://codereview.stackexchange.com/questions/139587/count-occurrences-of-an-element-in-a-list
  https://stackoverflow.com/questions/19521246/what-does-mean-do-in-haskell
  http://www.cse.chalmers.se/edu/course/TDA555/tourofprelude.html
  http://www.makechronicles.com/2012/11/11/first-steps-with-haskell-the-word-counter/
  https://stackoverflow.com/questions/5844347/haskell-accessing-a-specific-element-in-a-tuple#5844424
  https://gist.github.com/edalorzo/4670775
 -}

-- DO NOT MODIFY THE FOLLOWING LINE
module CompLing(wordCount, adjacentPairs, pairsCount, neighbours, mostCommonNeighbour) where

import Data.List  -- sort, group, ... and maybe more ...
import Test.HUnit -- provides testing framework
import PandP      -- provide sample text to play with (variable austin)

-- DO NOT CHANGE THESE TYPES

{- Represents a sentence as a list of words.
  INVARIANT: should be a non-empty list.
  -}
type Sentence = [String]

{- Represents a document.
  INVARIANT: should be a non-empty list.
  -}
type Document = [Sentence]

{- Represents a collection of words (in a document). For each word its occurence is also stored.
  INVARIANT: duplicates are not allowed, which means that a word may appear at most once.
            The integer representing the occurrence must be positiv.
  -}
type WordTally = [(String, Int)]

{- Represents a collection of word pairs (in a document). For each word its occurence is also stored.
  INVARIANT: -
  -}
type Pairs = [(String, String)]

{- Represents a collection of word pairs (in a document). For each pair of words its occurrence is also stored.
  INVARIANT: duplicates are not allowed, which means that a word pair may appear at most once.
            The integer representing the occurrence must be positiv.
  -}
type PairsTally = [((String, String), Int)]

-- DO NOT CHANGE THE TYPE SIGNATURES FOR THESE FUNCTIONS:

--   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --

{- 1'. elementAndItsOccurrence listOfIdenticalElements
    For each word in the argument, returns the number of occurrences.
  PRE:     the argument must be non-empty, and all the elements in the list must be equal.
  RETURNS: the element in the argument, and its occurrence.
  SIDE EFFECTS: -
  EXAMPLES:
    elementAndItsOccurrence ["a","a"] == ("a",2)
    elementAndItsOccurrence ["b","b","b"] == ("b",3)
-}
elementAndItsOccurrence :: [a] -> (a, Int)
elementAndItsOccurrence listOfEqualElems = (head listOfEqualElems, length listOfEqualElems)

{- 1. wordCount document
    For each word in the argument, returns the number of times the word occurrs.
  PRE:     -
  RETURNS: for each word in the argument, returns the word and its corresponding occurrence.
  SIDE EFFECTS: -
  EXAMPLES:
    wordCount [["a","a"]] == [("a",2)]
    wordCount [["a","b","a"]] == [("a",2),("b",1)]
    wordCount [["a","b"],["a"]] == [("a",2),("b",1)]
    wordCount [["by","umberto","eco"],["a","rose","is","a","rose","is","a","rose"]]
      == [("a",3),("by",1),("eco",1),("is",2),("rose",3),("umberto",1)]
-}
wordCount :: Document -> WordTally
wordCount [] = []
wordCount document = map elementAndItsOccurrence $ group $ sort $ concat document

--   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --

{- 2'. adjacentPairsIn1sentence document
    For each adjacent pair of words in the sentence, returns the pair. Duplicates included.
  PRE:     -
  RETURNS: list of all adjacent pairs of words appearing in the sentence, with duplicates present.
  SIDE EFFECTS: -
  EXAMPLES:
    adjacentPairsIn1sentence ["a","b"]
      == [("a","b")]
    adjacentPairsIn1sentence ["by","umberto","eco"]
      == [("by","umberto"),("umberto","eco")]
    adjacentPairsIn1sentence ["a","rose","is","a","rose","is","a","rose"]
      == [("by","umberto"),("umberto","eco"),("a","rose"),("rose","is"),("is","a"),
      ("a","rose"),("rose","is"),("is","a"),("a","rose")]
    adjacentPairsIn1sentence ["not","for","a","while"]
      == [("not","for"),("for","a"),("a","while")]
-}
adjacentPairsIn1sentence :: Sentence -> Pairs
adjacentPairsIn1sentence [] = []
adjacentPairsIn1sentence [w0] = []
adjacentPairsIn1sentence [w0,w1] = [(w0,w1)]
adjacentPairsIn1sentence (w:ws) = (w,head ws):adjacentPairsIn1sentence ws

{- 2. adjacentPairs document
    For each adjacent pair of words in the argument, returns the pair. Duplicates included.
  PRE:     -
  RETURNS: list of all adjacent pairs of words per sentence in the argument, with duplicates present.
  SIDE EFFECTS: -
  EXAMPLES:
    adjacentPairs [["a","b"]]
      == [("a","b")]
    adjacentPairs [["by","umberto","eco"],["a","rose","is","a","rose","is","a","rose"]]
      == [("by","umberto"),("umberto","eco"),("a","rose"),("rose","is"),("is","a"),
      ("a","rose"),("rose","is"),("is","a"),("a","rose")]
    adjacentPairs [["time","for","a","break"],["no"],["not","for","a","while"]]
      == [("time","for"),("for","a"),("a","break"),("not","for"),("for","a"),("a","while")]
-}
adjacentPairs :: Document -> Pairs
adjacentPairs [] = []
adjacentPairs (s:ss) = adjacentPairsIn1sentence s++adjacentPairs ss

--   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --

{- 3.a'. initPairsInSentence listOfWords
    Gets the first two words in the sentence.
  PRE:     -
  RETURNS: the first two words if they exist, otherwise two 'empty' words.
  SIDE EFFECTS: -
  EXAMPLES:
    initPairsInSentence ["time","for","a","break"] == ("time","for")
    initPairsInSentence ["no"] == ("","")
    initPairsInSentence ["not","yet"] == ("not","yet")
-}
initPairsInSentence :: Sentence -> (String,String)
initPairsInSentence []  = ("","")
initPairsInSentence [w0] = ("","")
initPairsInSentence [w0,w1] = (w0,w1)
initPairsInSentence (w0:w1:_) = initPairsInSentence [w0,w1]

{- 3.a. initialPairs document
    For each starting pair of words per sentence in the argument, returns the pair. Duplicates included.
  PRE:     -
  RETURNS: list of all starting pair of words per sentence in the argument, with duplicates present.
  SIDE EFFECTS: -
  EXAMPLES:
    initialPairs [["time","for","a","break"],["not","yet"],["not","yet"]]
      == [("time","for"),("not","yet"),("not","yet")]
    initialPairs [["time","for","a","break"],["no"],["not","yet"]]
      == [("time","for"),("not","yet")]
    initialPairs [["yet","not"]]
      == [("yet","not")]
-}
initialPairs :: Document -> Pairs
initialPairs [] = []
initialPairs (s:ss)
  | initPairsInSentence s==("","") = initialPairs ss
  | otherwise                      = initPairsInSentence s:initialPairs ss


{- 3.b. finalPairs document
    For each ending pair of words per sentence in the argument, returns the pair. Duplicates included.
  PRE:     -
  RETURNS: list of all ending pair of words per sentence in the argument, with duplicates present.
  SIDE EFFECTS: -
  EXAMPLES:
    finalPairs [["time","for","a","break"],["not","yet"],["not","yet"]]
      == [("break","a"),("not","yet"),("not","yet")]
    finalPairs [["time","for","a","break"],["no"],["not","yet"]]
      == [("a","break"),("not","yet")]
-}
finalPairs :: Document -> Pairs
finalPairs [] = []
finalPairs (s:ss)
  | finalPairsInSentence s==("","") = finalPairs ss
  | otherwise                       = finalPairsInSentence s:finalPairs ss
  where
    {- 3.b'. finalPairsInSentence listOfWords
        Gets the last two words in the sentence.
      PRE:     -
      RETURNS: the last two words if they exist, otherwise two 'empty' words.
      SIDE EFFECTS: -
      EXAMPLES:
        finalPairsInSentence ["time","for","a","break"] == ("a","break")
        finalPairsInSentence ["no"] == ("","")
        finalPairsInSentence ["not","yet"] == ("not","yet")
    -}
    finalPairsInSentence :: Sentence -> (String,String)
    finalPairsInSentence []  = ("","")
    finalPairsInSentence [w0] = ("","")
    finalPairsInSentence [w0,w1] = (w0,w1)
    finalPairsInSentence (w:ws) = finalPairsInSentence ws

--   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --

{- 4a'. sortedListFromTuple tuple
    The tuple is converted to a list and then sorted.
  PRE:     -
  RETURNS: sorted list consisting of the two elements of the tuple.
  SIDE EFFECTS: -
  EXAMPLES:
    sortedListFromTuple ("a","a") == ["a","a"]
    sortedListFromTuple ("a","b") == ["a","b"]
    sortedListFromTuple ("b","a") == ["a","b"]
-}
sortedListFromTuple :: (String, String) -> [String]
sortedListFromTuple (s0,s1) = sort [s0,s1]

{- 4b'. tupleFromList list
    The list is converted to a tuple.
  PRE:     the argument must contain exactly two elements.
  RETURNS: tuple consisting of the two elements of the list (in the same order as in the tuple).
  SIDE EFFECTS: -
  EXAMPLES:
    tupleFromList ["a","b"] == ("a","b")
    tupleFromList ["b","a"] == ("b","a")
-}
tupleFromList :: [String] -> (String, String)
tupleFromList [s0,s1] = (s0,s1)

{- 4c'. orderedTuple tuple
    For any tuple, returns a permutation of the tuple such that the elements are ordered.
  PRE:     -
  RETURNS: tuple consisting of the two elements of the argument - but now guaranteed to be in order.
  SIDE EFFECTS: -
  EXAMPLES:
    orderedTuple ("a","b") == ("a","b")
    orderedTuple ("b","a") == ("a","b")
    orderedTuple ("c","a") == ("a","c")
-}
orderedTuple :: (String, String) -> (String, String)
orderedTuple (s0,s1) = tupleFromList $ sortedListFromTuple (s0,s1)

{- 4d'. pairsAllOrdered listOfAdjacentPairs
    Keeps each pair of words in the argument - but now in guaranteed order.
  PRE:     -
  RETURNS: The same list of pairs - but now each pair of same elements have them in the same order.
  SIDE EFFECTS: -
  EXAMPLES:
    pairsAllOrdered [] == []
    pairsAllOrdered [("a","b"),("b","c"),("c","a"),("a","b")]
      == [("a","b"),("b","c"),("a","c"),("a","b")]
    (pairsAllOrdered $ adjacentPairs [["b","a","b"],["c"]])
      == [("a","b"),("a","b")]
    pairsAllOrdered [("big","bear"),("bear","big"),("big","dog")]
      == [("bear","big"),("bear","big"),("big","dog")]
-}
pairsAllOrdered :: [(String, String)] -> [(String, String)]
pairsAllOrdered [] = []
pairsAllOrdered (p:ps) = orderedTuple p:pairsAllOrdered ps

{- 4. pairsCount listOfPairs
    For each pair of words in the argument, returns the number of times the pair occurrs.
    PRE:   -
  RETURNS: for each word pair in the argument, returns the word pair ordered and its corresponding occurrence.
            ("b","a") and ("a","b") are considered to be the SAME pair - it is returned as ("a","b").
  SIDE EFFECTS: -
  EXAMPLES:
    pairsCount [] == []
    pairsCount [("a","b"),("b","c"),("c","a"),("a","b")]
      == [(("a","b"),2),(("a","c"),1),(("b","c"),1)]
    (pairsCount $ adjacentPairs [["b","a","b"],["c"]])
      == [(("a","b"),2)]
    (pairsCount $ adjacentPairs [["by","umberto","eco"],["a","rose","is","a","rose","is","a","rose"]])
      == [(("a","is"),2),(("a","rose"),3),(("by","umberto"),1),(("eco","umberto"),1),(("is","rose"),2)]
-}
pairsCount :: Pairs -> PairsTally
pairsCount pairs = map elementAndItsOccurrence $ group $ sort $ pairsAllOrdered pairs

--   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --

{- 5. neighbours tallyOfPairs wordToSearchFor
    For word in the second argument, gives all the neighbouring words that appear 
    with that word in the tally of pairs - along with the number of occurrences.
  PRE:     -
  RETURNS: for the word in the second argument, returns all the words that appear in the first
          argument with that word - along with the already given number of occurrences.
  SIDE EFFECTS: -
  EXAMPLES:
    neighbours [(("a","b"),2),(("c","d"),1)] "x"
      == []
    neighbours [(("a","b"),2),(("c","d"),1)] "a"
      == [("b",2)]
    neighbours [(("a","b"),2),(("c","d"),1)] "b"
      == [("a",2)]
    neighbours [(("bear","big"),2),(("big","dog"),1)] "big"
      ==  [("bear",2),("dog",1)]
    neighbours (pairsCount $ adjacentPairs [["by","umberto","eco"],
    ["a","rose","is","a","rose","is","a","rose"]]) "rose"
      == [("a",3),("is",2)]
-}
neighbours :: PairsTally -> String -> WordTally
neighbours pairsTally      "" = []
neighbours        []        w = []
neighbours (((w0,w1),j):ps) w
  | w == w0                   = (w1,j):neighbours ps w
  | w == w1                   = (w0,j):neighbours ps w
  | otherwise                 = neighbours ps w

--   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --

{- 6'. wordTallyMax tallyOfWords
    Gives the word that has the highest occurrence count in a tally of words.
  PRE:     -
  RETURNS: the word that has the highest occurrence count in the argument.
          Nothing if the argument is the empty list.
  SIDE EFFECTS: -
  EXAMPLES:
    wordTallyMax [] "wrong!" 0
      == Nothing
    wordTallyMax [("b",2)] "a" 3
      == Just "a"
    wordTallyMax [("b",2)] "a" 1
      == Just "b"
    wordTallyMax [("bear",2),("dog",3)] "wrong!" 0
      == Just "dog"
    wordTallyMax (neighbours (pairsCount $ adjacentPairs
    [["by","umberto","eco"],["a","rose","is","a","rose","is","a","rose"]]) "rose") "wrong!" 0
      == Just "a"
-}
wordTallyMax :: WordTally -> String -> Int -> Maybe String
wordTallyMax    []  word  maxi
  | maxi == 0                     = Nothing
  | otherwise                     = Just word
wordTallyMax ((w,j):ps) word maxi
  | j > maxi                      = wordTallyMax ps   w   j
  | otherwise                     = wordTallyMax ps word maxi

{- 6. mostCommonNeighbour tallyOfPairs wordToSearchFor
    Gives the word that occurs most frequently with a given word, based on a tally of pairs.
  PRE:     -
  RETURNS: the word that occurs most frequently with the second argument, given the first argument.
          Otherwise Nothing. If the searched for word is the empty string, again Nothing is returned.
  SIDE EFFECTS: -
  EXAMPLES:
    mostCommonNeighbour [(("a","b"),2),(("c","d"),1)] "x"
      == Nothing
    mostCommonNeighbour [(("a","b"),2),(("c","d"),1)] "a"
      == Just "b"
    mostCommonNeighbour [(("a","b"),2),(("c","d"),1)] "b"
      == Just "a"
    mostCommonNeighbour [(("bear","big"),2),(("big","dog"),1)] "big"
      ==  Just "bear"
    mostCommonNeighbour (pairsCount $ adjacentPairs [["by","umberto","eco"],
    ["a","rose","is","a","rose","is","a","rose"]]) "rose"
      == Just "a"
    mostCommonNeighbour (pairsCount $ adjacentPairs $ austin) "bennet"
      == Just "mr"
    mostCommonNeighbour (pairsCount $ adjacentPairs $ austin) "lucas"
      == Just "lady"
-}
mostCommonNeighbour :: PairsTally -> String -> Maybe String
mostCommonNeighbour pairsTally word = wordTallyMax (neighbours pairsTally word) "wrong!" 0

--   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --
