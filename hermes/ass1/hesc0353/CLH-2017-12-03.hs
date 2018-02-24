-- DO NOT MODIFY THE FOLLOWING LINE
module CompLing(wordCount, adjacentPairs, pairsCount, neighbours, mostCommonNeighbour) where

import Test.HUnit -- provides testing framework
import PandP      -- provide sample text to play with (variable austin)




-- DO NOT CHANGE THESE TYPES
type Sentence = [String]
type Document = [Sentence]
type WordTally = [(String, Int)]
type Pairs = [(String, String)]
type PairsTally = [((String, String), Int)]

-- DO NOT CHANGE THE TYPE SIGNATURES FOR THESE FUNCTIONS




{- wordCount Document -> wordtally
Counts words i a Documnet and returns it i a wordTally 
PRE:  A Document
RETURNS: A WordTally 
SIDE EFFECTS: - 
EXAMPLES:  WordTally [["a","b"]] -> [("b",1),("",1)]
-}

wordCount :: Document -> WordTally
wordCount doc = 
                let 
                    list = concat doc 
                    wTally  = wCountAux list [("*",-1)] 
                in
                    init wTally


wCountAux:: [String] ->WordTally ->WordTally
wCountAux [] wordT  = wordT
wCountAux  list@(x:xs) wordT =
                                let
                                    shortList = [ t  |  t <- list , t /=x ] -- the list without x 
                                    wordT2 = (x, (length list - length shortList)):wordT -- differense = nr. of words 
                                in
                                wCountAux shortList wordT2

{- adjacentPairsAux Document -> Pairs
Returns a pairList with all pairs (sentencewise) from the Document
PRE: Document
RETURNS: Pairs 
SIDE EFFECTS: - 
EXAMPLES:  adjacentPairsAux [["a","b","c"],["d","e","f"]] -> [("a,b"),("b","c"),("d","e"),("e","f")]
-}

adjacentPairs :: Document -> Pairs
adjacentPairs doc = 
                    let 
                        pairsList = adjacentPairsAux doc [("*","*")] 
                    in
                        tail pairsList

                        
adjacentPairsAux:: Document-> Pairs ->Pairs
adjacentPairsAux [] pairs =  pairs
adjacentPairsAux (x:xs) pairs =
                            let 
                                pr = setPairs x pairs
                            in  adjacentPairsAux xs pr

setPairs::Sentence -> Pairs -> Pairs
setPairs [] ap = undefined
-- Räknar antal par i en mening
setPairs [a] ap = ap
setPairs (x:xs) ap = setPairs xs (ap ++ [(x, head xs)])


{- PairsCount Pairs -> PairsTally
Count the number of pair of each sort. Note that pair (a,b) == (b,a) in this function.  
PRE:  Pairs
RETURNS: Pair Tally 
SIDE EFFECTS: - 
EXAMPLES:  PairTally [("a","b"),("c","d"),("b","a")] -> [((a,b), 2),(("c","d"),1)]
] -}


pairsCount :: Pairs -> PairsTally 

pairsCount p =
            let 
                pairList = pairsCountAux p [(("*","*"),-1)]
            in  
                init pairList

pairsCountAux:: Pairs ->PairsTally ->PairsTally


pairsCountAux [] pt = pt

pairsCountAux inPairs@(x : xs) pt  =
                                        let 
                                            xList = [t | t <- inPairs ,t ==  x || t == (snd x, fst x)] 
                                            noX = length xList
                                            pt2 = (x, noX):pt
                                        in
                                            pairsCountAux xs pt2


{- Initial Pairs Document -> Pairs 
    Creates a pairlist of the two first words i each Sentence in the Document
PRE:  Document with no sentences shorter than two words!
RETURNS: Pairs
SIDE EFFECTS: - 
EXAMPLES: initialPairsAux [["a","b","c"], ["c","d"]] -> [("c","d"),("a","b")]
            initialPairs ["a","b","c"], ["a"], [] -> "Error!"
-}
initialPairs::Document -> Pairs

initialPairs doc = 
                let  
                    pairs= initialPairsAux doc [("*","*")]
                in 
                    init pairs
                    
initialPairsAux::Document->Pairs->Pairs

initialPairsAux [] pairs = pairs

initialPairsAux (x:xs) pairs    
    | (length x) > 1 =
                            let
                                p1 = head x
                                p2 = head (tail x)
                                p3 = (p1 , p2):pairs
                            in 
                                initialPairsAux xs p3
    | otherwise = initialPairsAux xs pairs
                        


{- finalPairs Document -> Pairs 
    Creates a pairlist of the two last words i each Sentence of the Document
PRE:  Document with no sentences shorter than two words!
RETURNS: Pairs
SIDE EFFECTS: - 
EXAMPLES: finalPairsAux [["a","b","c"], ["c","d"]] -> [("b","c"),("c","d")]
finalPairs ["a","b","c"], ["a"] -> [("b","c")]
-}              
finalPairs::Document -> Pairs
finalPairs doc = let 
                    fpairs =finalPairsAux doc [("*","*")]
                in 
                    init fpairs
                        
                        
finalPairsAux::Document-> Pairs-> Pairs

finalPairsAux [] pairs = pairs
finalPairsAux doc@(x:xs) pairs 
    | length (last doc) > 1 = 
        
                    let 
                        p1 = last(last doc)
                        p2 = last (init (last doc))
                        p3 = (p2,p1):pairs
                    in 
                        finalPairsAux (init doc) p3
    | otherwise = finalPairsAux (init doc) pairs
                        


{- neighbours  PairsTally -> String -> WordTally 
    Counts the number of times the String appears together with another word in the PairsTally 
PRE:  PairsTally, String
RETURNS: wordTally 
SIDE EFFECTS: - 
EXAMPLES:  neigbours [(("a","b"],2),(("c","a"),3)] "a" -> [("c",3),("b",2)]
-}


neighbours :: PairsTally -> String -> WordTally
neighbours pt string = 
                        let 
                            nb = neighboursAux (pt ++[(("*","*"),-1)])  string [("*",-1)]
                        in
                            init nb

neighboursAux:: PairsTally -> String -> WordTally -> WordTally
neighboursAux [] st wt = wt
neighboursAux [(("*","*"),-1)] st wt    = wt


neighboursAux (x:xs) string wTally
        | string == fst (fst x) = neighboursAux xs string  ( (snd (fst x), snd (x)):wTally)
        | string == snd (fst x) = neighboursAux xs string ( (fst (fst x), snd (x)) :wTally)
        | otherwise = neighboursAux xs string wTally


{- mostCommonNeighbour PairsTally -> String -> Maybe String
    Returns the most common neighbour to the String i the PairsTally or nothing if there isn´t any. 
PRE:  PairsTally
RETURNS: Maybe String  
SIDE EFFECTS: - 
EXAMPLES:  MostCommonNeighbour [(("a","b"],2),(("c","a"),3)] "a" = Just "c"
            mostCommonNeighbour [(("a","b"],2),(("c","a"),3)] "y" = Nothing
-}


mostCommonNeighbour :: PairsTally -> String ->  Maybe String
mostCommonNeighbour pt st =  mostCommonNeighbourAux pt st "*" 0

mostCommonNeighbourAux :: PairsTally ->String -> String ->Int ->Maybe String
mostCommonNeighbourAux [] st returSt max =  
                                            if returSt == "*" 
                                                then Nothing
                                                else Just returSt

mostCommonNeighbourAux  (x:xs) st returSt max

    | fst (fst x) == st =  
                            if snd x > max
                                then  
                                    let
                                        max = snd x 
                                        returSt = snd (fst x)
                                    in  
                                        mostCommonNeighbourAux xs st returSt max
                                else 
                                    mostCommonNeighbourAux xs st returSt max
    
    |snd (fst x) == st = 
                            if snd x > max
                            then
                                let
                                    max = snd x 
                                    returSt = fst (fst x)
                                in  
                                    mostCommonNeighbourAux xs st returSt max
                            else
                                mostCommonNeighbourAux xs st returSt max
    
    |otherwise = mostCommonNeighbourAux xs st returSt max
    


-- Test Cases
-- feel free to add other test cases here. an independent set of
-- test cases will be used when grading your code

-- wordCount
test1 = TestCase $ assertEqual "wordCount []" [] (wordCount [])
test2 = TestCase $ assertBool "wordCount [[\"a\",\"b\"],[\"a\"]]" (elem ("a",2) (wordCount [["a","b"],["a"]]))

-- adjacentPairs, initialPairs, finalPairs
test3 = TestCase $ assertEqual "adjacentPairs [[\"foo\"],[\"bar\"]]" [] (adjacentPairs [["foo"],["bar"]]) 

test3a = TestCase $ assertEqual "initialPairs" [("a","b")] (initialPairs [["a","b","a"],["c"]])
                      
test3b = TestCase $ assertEqual "finalPairs" [("b","a")] (finalPairs [["a","b","a"],["c"]])
                      

-- pairsCount
test4 = TestCase $ assertBool "pairsCount simple" 
            (elem (("a","b"), 2) (pairsCount [("a","b"),("c","d"),("a","b")]))
test5 = TestCase $ assertBool "pairsCount tricky" 
             (let x = pairsCount (adjacentPairs [["a","b","a"],["c"]]) in 
                      elem (("a","b"), 2) x || elem (("b","a"), 2) x)

-- neighbours
test6 = TestCase $ assertEqual "neighbours left" [("b",2)] 
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "a") 

test7 = TestCase $ assertEqual "neighbours left" [("a",2)]
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "b") 

-- mostCommonNeighbour
test8 = TestCase $ assertEqual "mostCommonNeighbour text \"the\"" (Just "fun") 
                                                                  (mostCommonNeighbour input "the") 
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

test9 = TestCase $ assertEqual "mostCommonNeighbour text \"spam\"" 
                      Nothing (mostCommonNeighbour input "spam")
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

-- testing the PandP.austin text
test10 = TestCase $ assertEqual "mostCommonNeighbour of \"bennet\"" 
            (Just "mr") (mostCommonNeighbour (pairsCount $ adjacentPairs $ austin) "bennet") 

-- for running all the tests (type "runtests" within ghci --- without the quotes)
runtests = runTestTT $ TestList [test1, test2, test3, test3a, test3b, test4, test5, test6, test7,test8,test9,test10]
runtests0_8 = runTestTT $ TestList [test1, test2, test3, test3a, test3b, test4, test5, test6, test7]


