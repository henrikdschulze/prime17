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
 -}

-- DO NOT MODIFY THE FOLLOWING LINE
module CompLing(wordCount, adjacentPairs, pairsCount, neighbours, mostCommonNeighbour) where

import Data.List  -- sort, ...
import Test.HUnit -- provides testing framework
import PandP      -- provide sample text to play with (variable austin)
import Arose      -- provide sample text to play with (variable umbertoeco)

-- DO NOT CHANGE THESE TYPES
type Sentence = [String]
type Document = [Sentence]
type WordTally = [(String, Int)]
type Pairs = [(String, String)]
type PairsTally = [((String, String), Int)]
-- -- type Word = String            --
-- type Sentence = [String]        --
-- type Document = [Sentence]
-- type WordTally = [(String,Int)]
-- -- type Pair = (String,String)
-- type Pairs = [(String,String)]
-- type PairsTally = [((String,String),Int)]

-- DO NOT CHANGE THE TYPE SIGNATURES FOR THESE FUNCTIONS:

{- 1. wordCount doc
  For each word in doc, returns the number of times the word is found.
  PRE:     -
  RETURNS: for each word in the argument list, returns the word and its corresponding occurrence.
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
wordCount doc = map headAndLength $ group $ sort $ concat doc
  where
    headAndLength listOfEqualWords = (head listOfEqualWords, length listOfEqualWords)

{- 2. adjacentPairs doc
  For each adjacent pair of words in doc, returns the pair. Duplicates included.
  PRE:     -
  RETURNS: list of all adjacent pairs of words appearing in the document, with duplicates present.
  SIDE EFFECTS: -
  EXAMPLES:
    adjacentPairs [["a","b"]] == [("a","b")]
    adjacentPairs [["by","umberto","eco"],["a","rose","is","a","rose","is","a","rose"]]
      == [("by","umberto"),("umberto","eco"),("a","rose"),("rose","is"),("is","a"),
      ("a","rose"),("rose","is"),("is","a"),("a","rose")]
-}
adjacentPairs :: Document -> Pairs
-- adjacentPairs = undefined  -- remove "undefined" and write your function here
adjacentPairs = undefined  -- remove "undefined" and write your function here

{- 3.a. initialPairs doc
  For each starting pair of words per sentence in doc, returns the pair. Duplicates included.
  PRE:     -
  RETURNS: list of all starting pair of words per sentence in the document, with duplicates present.
  SIDE EFFECTS: -
  EXAMPLES:
    initialPairs [["time","for","a","break"],["not","yet"],["not","yet"]]
      == [("time","for"),("not","yet"),("not","yet")]
    initialPairs [["time","for","a","break"],["no"],["not","yet"]]
      == [("time","for"),("not","yet")]
-}
initialPairs :: Document -> Pairs
initialPairs [] = []
initialPairs (s:ss)
  | (firstTwo s)==("","") = initialPairs ss
  | otherwise             = (firstTwo s):initialPairs ss
  where
    {- 3.a'. firstTwo listOfWords
      Gets the first two words in the sentence.
      PRE:     -
      RETURNS: the first two words if they exist, otherwise two 'empty' words.
      SIDE EFFECTS: -
      EXAMPLES:
        firstTwo ["time","for","a","break"] == ("time","for")
        firstTwo ["no"] == ("","")
        firstTwo ["not","yet"] == ("not","yet")
    -}
    firstTwo :: Sentence -> (String,String)
    firstTwo []  = ("","")
    firstTwo [w0] = ("","")
    firstTwo [w0,w1] = (w0,w1)
    firstTwo (w0:w1:_) = firstTwo [w0,w1]

-- {- 3.a. initialPairs doc
--   For each starting pair of words per sentence in doc, returns the pair. Duplicates included.
--   PRE:     -
--   RETURNS: list of all starting pair of words per sentence in the document, with duplicates present.
--   SIDE EFFECTS: -
--   EXAMPLES:
--     initialPairs [["time","for","a","break"],["not","yet"],["not","yet"]]
--       == [("time","for"),("not","yet"),("not","yet")]
--     initialPairs [["time","for","a","break"],["no"],["not","yet"]]
--       == [("time","for"),("not","yet")]
-- -}
-- initialPairs :: Document -> Pairs
-- initialPairs [] = []
-- initialPairs (s:ss)
--   | (firstTwo s)==("","") = initialPairs ss
--   | otherwise             = (firstTwo s):initialPairs ss
--   where
--     {- 3.a'. firstTwo listOfWords
--       Gets the first two words in the sentence.
--       PRE:     -
--       RETURNS: the first two words if they exist, otherwise two 'empty' words.
--       SIDE EFFECTS: -
--       EXAMPLES:
--         firstTwo ["time","for","a","break"] == ("time","for")
--         firstTwo ["no"] == ("","")
--         firstTwo ["not","yet"] == ("not","yet")
--     -}
--     firstTwo :: Sentence -> (String,String)
--     firstTwo []  = ("","")
--     firstTwo [w0] = ("","")
--     firstTwo [w0,w1] = (w0,w1)
--     firstTwo (w0:w1:_) = firstTwo [w0,w1]

{- 3.b. finalPairs doc
  For each ending pair of words per sentence in doc, returns the pair. Duplicates included.
  PRE:     -
  RETURNS: list of all ending pair of words per sentence in the document, with duplicates present.
  SIDE EFFECTS: -
  EXAMPLES:
    finalPairs [["time","for","a","break"],["not","yet"],["not","yet"]]
      == [("time","for"),("not","yet"),("not","yet")]
    finalPairs [["time","for","a","break"],["no"],["not","yet"]]
      == [("time","for"),("not","yet")]
-}
finalPairs :: Document -> Pairs
finalPairs [] = []
finalPairs (s:ss) = initialPairs (reverse s:ss)

pairsCount :: Pairs -> PairsTally
pairsCount = undefined  -- remove "undefined" and write your function here

neighbours :: PairsTally -> String -> WordTally
neighbours = undefined  -- remove "undefined" and write your function here

mostCommonNeighbour :: PairsTally -> String -> Maybe String
mostCommonNeighbour = undefined  -- remove "undefined" and write your function here


-- Test Cases.
-- Example: to run 'test0' only, in ghci type <runTestTT $ TestList [test0]> and press Enter.
-- To run test2, only, in ghci type <runTestTT $ TestList [test0]> and press Enter.
-- Feel free to add other test cases here. An independent set of
-- test cases will be used when grading your code

-- wordCount:
test0 = TestCase $ assertEqual "wordCount []" [] (wordCount [])
test1 = TestCase $ assertBool "wordCount [[\"a\",\"b\"],[\"a\"]]" (elem ("a",2) (wordCount [["a","b"],["a"]]))

-- adjacentPairs:
test2 = TestCase $ assertEqual "adjacentPairs [[\"foo\"],[\"bar\"]]" [] (adjacentPairs [["foo"],["bar"]])

-- small example with 4 words
-- pairsCount
test3 = TestCase $ assertBool "pairsCount simple" 
            (elem (("a","b"), 2) (pairsCount [("a","b"),("c","d"),("a","b")]))
test4 = TestCase $ assertBool "pairsCount tricky" 
             (let x = pairsCount (adjacentPairs [["a","b","a"],["c"]]) in 
                      elem (("a","b"), 2) x || elem (("b","a"), 2) x)

-- neighbours
test5 = TestCase $ assertEqual "neighbours left" [("b",2)] 
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "a")

test6 = TestCase $ assertEqual "neighbours left" [("a",2)]
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "b")

-- mostCommonNeighbour
test7 = TestCase $ assertEqual "mostCommonNeighbour text \"the\"" (Just "fun")
                                                                  (mostCommonNeighbour input "the")
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

test8 = TestCase $ assertEqual "mostCommonNeighbour text \"spam\""
                      Nothing (mostCommonNeighbour input "spam")
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

-- testing the PandP.austin text
test9 = TestCase $ assertEqual "mostCommonNeighbour of \"bennet\""
            (Just "mr") (mostCommonNeighbour (pairsCount $ adjacentPairs $ austin) "bennet")
runtest9 = runTestTT $ TestList [test9]

test10 = TestCase $ assertEqual "initialPairs" [("a","b")] (initialPairs [["a","b","a"],["c"]])

test11 = TestCase $ assertEqual "finalPairs" [("b","a")] (finalPairs [["a","b","a"],["c"]])

-- testing the Arose.umbertoeco text
test21 = TestCase $ assertEqual "mostCommonNeighbour of \"rose\""
            (Just "a") (mostCommonNeighbour (pairsCount $ adjacentPairs $ umbertoeco) "rose")
runtest21 = runTestTT $ TestList [test21]

-- for running all the tests (type "runtests0_9" within ghci --- without the quotes)
runtests0_9 = runTestTT $ TestList [test0,test1, test2, test3, test4, test5, test6, test7,test8,test9]
