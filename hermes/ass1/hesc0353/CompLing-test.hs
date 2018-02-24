-- DO NOT MODIFY THE FOLLOWING LINE
module CompLing(wordCount, adjacentPairs, pairsCount, neighbours, mostCommonNeighbour) where
import Test.HUnit -- provides testing framework
import PandP      -- provide sample text to play with (variable austin)
import TNofTR     -- provide sample text to play with (variable umbertoeco)

-- DO NOT CHANGE THESE TYPES
type Sentence = [String]
type Document = [Sentence]
type WordTally = [(String, Int)]
type Pairs = [(String, String)]
type PairsTally = [((String, String), Int)]

-- DO NOT CHANGE THE TYPE SIGNATURES FOR THESE FUNCTIONS:

wordCount :: Document -> WordTally
-- wordCount = undefined  -- remove "undefined" and write your function here
wordCount [] = []

adjacentPairs :: Document -> Pairs
adjacentPairs = undefined  -- remove "undefined" and write your function here

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

-- wordCount
test0 = TestCase $ assertEqual "wordCount []" [] (wordCount [])
test1 = TestCase $ assertBool "wordCount [[\"a\",\"b\"],[\"a\"]]" (elem ("a",2) (wordCount [["a","b"],["a"]]))

-- adjacentPairs
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
runtest7 = runTestTT $ TestList [test7]

test8 = TestCase $ assertEqual "mostCommonNeighbour text \"spam\""
                      Nothing (mostCommonNeighbour input "spam")
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]
runtest8 = runTestTT $ TestList [test8]

-- testing the PandP.austin text
test9 = TestCase $ assertEqual "mostCommonNeighbour of \"bennet\""
            (Just "mr") (mostCommonNeighbour (pairsCount $ adjacentPairs $ austin) "bennet")
runtest9 = runTestTT $ TestList [test9]

-- testing the TNofTR.umbertoeco text
test21 = TestCase $ assertEqual "mostCommonNeighbour of \"rose\""
            (Just "a") (mostCommonNeighbour (pairsCount $ adjacentPairs $ umbertoeco) "rose")
runtest21 = runTestTT $ TestList [test21]

-- for running all the tests (type "runtests0_9" within ghci --- without the quotes)
runtests0_9 = runTestTT $ TestList [test0,test1, test2, test3, test4, test5, test6, test7,test8,test9]
