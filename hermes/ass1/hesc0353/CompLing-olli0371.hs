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

-- DO NOT CHANGE THE TYPE SIGNATURES FOR THESE FUNCTIONS
{-tar bort element w ur listan-}

{-
In: Ett sträng w och en lista av strängar
Ut: samma lista men med alla w borttagna.
förkrav:--
-}
removeElement ::String -> [String] -> [String]
removeElement w list = [x | x <-list, x /= w] --lägg till alla element utom w i returlistan

{-
In: Sträng och en WordTally (wt)
ut: En WordTally som har räknat förekomsten av samtliga ord i listan
förkrav: Inwt:n bör vara tom.
-}
wCountAux:: [String] ->WordTally ->WordTally
wCountAux [] wt = wt
-- Lägg alla element som är lika med x i en lista. Längden på den listan blir antalet
-- därefter tar vi bort alla kopior av x som förekommer.
wCountAux  (x:xs) wt =
        let wt2 = ((x ,length [ t  |  t <- (x:xs), t==x ]) :wt) in wCountAux (removeElement x xs) wt2



{-
in: Ett Document
ut: En  WordTally med Dokumentets samtliga ord räknade.
förkrav: -
-}
wordCount :: Document -> WordTally
-- lägger till en flagga ("*",-1) för att kunna skicka en tuple. Skapar sedan lista utan denna.
wordCount doc =  [t | t  <- (wCountAux (concat doc) [("*",-1)])  , t /= ("*",-1) ]


{-
in: En sträng och en (tom) PairsLista
ut: en PairsLista med samtliga par i inlistan
förkrav:
-}
adjacentPairsAux:: Document-> Pairs ->Pairs
adjacentPairsAux [] ap =  ap
-- skickar varje lista i dokumentet till setPairs för parrräkning
adjacentPairsAux (x:xs) ap =
                                        let ap2 = setPairs x ap in  adjacentPairsAux xs ap2
{-
in: En mening
ut: en pairsTuple betsånende av paren i meningen
förkrav: -
-}
setPairs::Sentence -> Pairs -> Pairs
setPairs [] ap = undefined
-- Räknar antal par i en mening
setPairs [a] ap = ap

setPairs (x:xs) ap = setPairs xs (ap ++ [(x, head xs)])


{-
in: Ett dokument
ut: en pairsTuple betsående av paren i dokumentet
förkrav: dokumentet får inte innehålla strängen "*"
-}

adjacentPairs :: Document -> Pairs
adjacentPairs doc =  [t | t  <- (adjacentPairsAux doc [("*","*")])  , t /= ("*","*") ]



{-******************************************************************************-}
pairsCountAux:: Pairs ->PairsTally ->PairsTally
pairsCountAux [] pt = pt
-- sätter ihop en lisa av alla par där x ingår på första eller sista plats och räknar längden på denna
-- samt lägger den till vår svarstally
pairsCountAux (x : xs) pt   =  pairsCountAux  ( [ t |  t <- (xs)  ,    t /=  x  ,  t /=  (snd x, fst x)       ]      )   (   (    ( x) ,  (length [ t |  t <- (x:xs)  ,    t ==  x || t == (snd x, fst x)])   ) :pt      )

 {-
 in: Pairs
 ut: En pairsTallylista för inListan
 förkrav: -
 -}

pairsCount :: Pairs -> PairsTally
pairsCount p = [t | t <-  pairsCountAux p  [(("*","*"),-1)], t/= (("*","*"),-1) ]


neighboursAux:: PairsTally -> String -> WordTally -> WordTally
neighboursAux [] st wt = wt
neighboursAux [(("*","*"),-1)]  st wt    = wt

-- Om insträngen ligger i första eller andra positionen i parListan
-- så läggs det andra elementet till en wordtally första element och talet till andra.
neighboursAux (x:xs) st wt
        | st == fst (fst x) = neighboursAux xs st  ( (snd (fst x), snd (x)):wt)
        | st == snd (fst x) = neighboursAux xs st ( (fst  (fst x),   snd (x)) :wt)
        | otherwise = neighboursAux xs st wt


neighbours :: PairsTally -> String -> WordTally
neighbours pt st = [t | t <- neighboursAux  (pt ++[(("*","*"),-1)])  st  [("*",-1)],   t /= ("*",-1)]

mostCommonNeighbour :: PairsTally -> String -> Maybe String
mostCommonNeighbour = undefined  -- remove "undefined" and write your function here



-- Test Cases
-- feel free to add other test cases here. an independent set of
-- test cases will be used when grading your code

-- wordCount
test1 = TestCase $ assertEqual "wordCount []" [] (wordCount [])
test2 = TestCase $ assertBool "wordCount [[\"a\",\"b\"],[\"a\"]]" (elem ("a",2) (wordCount [["a","a","b"],["a"]]))

-- adjacentPairs
test3 = TestCase $ assertEqual "adjacentPairs [[\"foo\"],[\"bar\"]]" [] (adjacentPairs [["foo"],["bar"]])

-- test3a = TestCase $ assertEqual "initialPairs" [("a","b")] (initialPairs [["a","b","a"],["c"]])
-- 
-- test3b = TestCase $ assertEqual "finalPairs" [("b","a")] (finalPairs [["a","b","a"],["c"]])

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

-- testing the TNofTR.umbertoeco text
test21 = TestCase $ assertEqual "mostCommonNeighbour of \"rose\"" 
            (Just "a") (mostCommonNeighbour (pairsCount $ adjacentPairs $ umbertoeco) "rose") 

-- for running all the tests (type "runtests" within ghci --- without the quotes)
-- OLD! runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7,test8,test9,test10]
runtest = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7,test8,test9,test10]
-- runtests0_6 = runTestTT $ TestList [test1, test2, test3, test3a, test3b, test4, test5]
-- runtests = runTestTT $ TestList [test1, test2, test3, test3a, test3b, test4, test5, test6, test7,test8,test9,test10]
