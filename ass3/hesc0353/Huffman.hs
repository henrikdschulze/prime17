{-
  Uppsala University - 2018 Spring
  Program Design and Data Structures, 1DL201
  Assignment 3 - Data Compression
  Copyright 2018 Max Gabrielsson & Henrik Alfken(Schulze), Group G 31

['Submit (your modified version of) Huffman.hs via the Student Portal. Do not change the file name.']

Inspired by:
  https://studentportalen.uu.se/uusp-webapp/auth/webwork/filearea/download.action?nodeId=1107568&toolAttachmentId=208914&usg=AOvVaw2-8k80Y95qTGvFjA1mxLfs
  http://zvon.org/other/haskell/Outputchar/chr_f.html
  Henrik's solution to assignment 1 of this very same PKD course
-}
--------------------------------------------------------------------------------
-- DO NOT MODIFY THE FOLLOWING LINES
module Huffman(HuffmanTree, characterCounts, huffmanTree, codeTable, compress, decompress) where
import Data.List
import Data.Maybe
import PriorityQueue
import Table
import Test.HUnit
import Test.QuickCheck

{- a bit code (of a character or string) is represented by a list of Booleans
  INVARIANT:
    the bit code is a concatenation of (0 or more) valid code words for some Huffman tree
-}
type BitCode = [Bool]
-- END OF DO NOT MODIFY ZONE
--------------------------------------------------------------------------------

{- From the instructions PDF.
  2. Download the file Huffman.hs from the Student Portal. Define a data type HuffmanTree
  to represent Huffman trees, add missing comments, and implement the following functions:
    characterCounts :: String -> Table Char Int
    huffmanTree :: Table Char Int -> HuffmanTree
    codeTable :: HuffmanTree -> Table Char BitCode
    compress :: String -> (HuffmanTree, BitCode)
    decompress :: HuffmanTree -> BitCode -> String
  Make sure that compression and decompression are inverse operations, i.e., that
      let (h, bits) = compress s in decompress h bits
  evaluates to s for all possible input strings. Pay special attention to edge cases, such as
  "" or "xxx", and if necessary adjust your implementation so that it handles these cases
  correctly.
  EXAMPLES:
    let (h, bits) = compress "hello"    in decompress h bits
    let (h, bits) = compress "treeess"  in decompress h bits
    let (h, bits) = compress ""         in decompress h bits
    let (h, bits) = compress "xxx"      in decompress h bits
    let (h, bits) = compress "this is an example of a huffman tree"
                                        in decompress h bits
-}


{- Represents a Huffman tree for some string.
  INVARIANT:
    Should meet the specifics of a Huffman tree, such as being a binomial heap.
    For a string containing only one (type of) character, the corresponding Huffman tree
    should consist of only one leaf containing (a pair of) the character and its occurrence.
    For an empty string, the corresponding Huffman tree should be a leaf for which the
    occurrence (Int) value is zero.
-}
data HuffmanTree = Leaf (Char,Int) | Node HuffmanTree Int HuffmanTree
  deriving(Eq,Show)


-- - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- Start of <characterCounts> and its helper functions.
-- - - - - - - - - - - - - - - - - - - - - - - - - - - -
{- characterCounts s
    For each character in the argument, returns the character and its occurrence.
  PRE:     -
  RETURNS: a table that maps each character that occurs in s to the number of times
           the character occurs in s.
  SIDE EFFECTS: -
  EXAMPLES:
    characterCounts ""
      --> T []
    characterCounts "xxx"
      --> T [('x',3)]
    characterCounts "hello"
      --> T [('o',1),('l',2),('e',1),('h',1)]
    characterCounts "treeeaa"
      --> T [('t',1),('r',1),('e',3),('a',2)]
    characterCounts "yyyrghyy"
      --> T [('y',5),('r',1),('h',1),('g',1)]
-}
characterCounts :: String -> Table Char Int
characterCounts [] = Table.empty
characterCounts s = listToTable $ charactersCounted s


{- charactersCounted s
    For each character in s, returns the character and its occurrence in s.
  PRE:     -
  RETURNS: each character in s along with its occurrence.
  SIDE EFFECTS: -
  EXAMPLES:
    charactersCounted ""
      == []
    charactersCounted "xxx"
      == [('x',3)]
    charactersCounted "hello"
      == [('h',1),('e',1),('l',2),('o',1)]
    charactersCounted "treeeaa"
      == [('t',1),('r',1),('e',3),('a',2)]
    charactersCounted "yyyrghyy"
      == [('g',1),('h',1),('r',1),('y',5)]
-}
charactersCounted :: [Char] -> [(Char, Int)]
charactersCounted [] = []
charactersCounted s = map countoccurrence $ group $ sort s


{- countoccurrence listOfElements
    For each element in the argument, returns the number of times it occurrs.
  PRE:     listOfElements must be non-empty, and all the elements in the list must be equal.
  RETURNS: the element in the argument, and its occurrence.
  SIDE EFFECTS: -
  EXAMPLES:
    countoccurrence [7,7]
      == (7,2)
    countoccurrence ['b','b','b']
      == ('b',3)
-}
countoccurrence :: [a] -> (a, Int)
countoccurrence listOfEqualElems = (head listOfEqualElems, length listOfEqualElems)


{- listToTable listOfKeyValuePairs
    For each element in the argument, inserts it into the resulting Table.
  PRE:     each element in the argument must be a key-value pair.
  RETURNS: a table into which every key-value pair in the argument has been inserted.
  EXAMPLES:
    listToTable [('x', 3)]
      --> T [('x',3)]
    listToTable [('h',1),('e',1),('l',2),('o',1)]
      --> T [('o',1),('l',2),('e',1),('h',1)]
    listToTable [('t',1),('r',1),('e',3),('a',2)]
      --> T [('a',2),('e',3),('r',1),('t',1)]
-}
listToTable :: Eq k => [(k,v)] -> Table k v
-- VARIANT: length listOfKeyValuePairs
listToTable [] = Table.empty
listToTable ((k,v):listOfKeyValuePairs) = 
  Table.insert (listToTable listOfKeyValuePairs) k v
-- - - - - - - - - - - - - - - - - - - - - - - - - - -
-- End of <characterCounts> and its helper functions.
-- - - - - - - - - - - - - - - - - - - - - - - - - - -
-- Testing characterCounts:
test1 = TestCase $ assertEqual "characterCounts 'this is an example of a huffman tree'"
          (Just 7) (Table.lookup (characterCounts "this is an example of a huffman tree") ' ')
test1a = TestCase $ assertEqual "characterCounts 'hello'"
          (Just 2) (Table.lookup (characterCounts "hello") 'l')
test1b = TestCase $ assertEqual "characterCounts 'xxx'"
          (Just 3) (Table.lookup (characterCounts "xxx") 'x')
test1c = TestCase $ assertEqual "characterCounts \"\""
          Nothing (Table.lookup (characterCounts "") '\NUL')
testCharacterCounts1 = runTestTT $ TestList [test1,test1a,test1b,test1c]


-- - - - - - - - - - - - - - - - - - - - - - - - - -
-- Start of <huffmanTree> and its helper functions.
-- - - - - - - - - - - - - - - - - - - - - - - - - -
{- huffmanTree t
    For the given string, returns a corresponding Huffman tree.
  PRE:     t maps each key to a positive value.
  RETURNS: a Huffman tree based on the character counts in t.
  EXAMPLES:
    huffmanTree (listToTable [('\NUL',0)])
      == Leaf ('\NUL',0)
    huffmanTree (listToTable [('x',3)])
      == Leaf ('x',3)
    huffmanTree (listToTable [('h',1),('e',1),('l',2),('o',1)])
      == Node(Leaf('l',2))5(Node(Leaf('h',1))3(Node(Leaf('o',1))2(Leaf('e',1))))
-}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree t =
  oneHuffmanTree $ Table.iterate t (insertInPrioQ) PriorityQueue.empty

{- oneHuffmanTree q
    For the given priority qeueu, returns a single corresponding Huffman tree.
  PRE:     -
  RETURNS: a single (_one_) Huffman tree (recursively) constructed
           from all the Huffman trees in the given priority queue (q).
  EXAMPLES:
    oneHuffmanTree (insertInPrioQ PriorityQueue.empty ('\NUL',0))
      == Leaf ('\NUL',0)
    oneHuffmanTree (insertInPrioQ PriorityQueue.empty ('x',3))
      == Leaf ('x',3)
    oneHuffmanTree (insertInPrioQ (insertInPrioQ (insertInPrioQ
        (insertInPrioQ PriorityQueue.empty ('o',1)) ('l',2)) ('e',1)) ('h',1))
      == Node(Leaf('l',2))5(Node(Leaf('h',1))3(Node(Leaf('o',1))2(Leaf('e',1))))
-}
oneHuffmanTree :: PriorityQueue HuffmanTree -> HuffmanTree
-- VARIANTS: length q (or length qq)
oneHuffmanTree priorityQ =
  if is_empty q then x
  else oneHuffmanTree $ PriorityQueue.insert qq (Node x (p+pq) xq, p+pq)
    where ((x,p), q)     = least priorityQ  -- On this line, q has at least one element.
          ((xq, pq), qq) = least q


{- insertInPrioQ priorityQ keyValuePair
    Creates a leaf of the given key-value pair and inserts it into the given priority queue.
  PRE:     for each given key-value pair the value must be positive.
  RETURNS: a priority queue into which a HuffmanTree leaf created from the key-value pair
           has been inserted.
  EXAMPLES:
    insertInPrioQ PriorityQueue.empty ('x',3)
      --> BinoHeap [Node 0 3 (Leaf ('x',3)) []]
    insertInPrioQ (insertInPrioQ PriorityQueue.empty ('h',1)) ('e',1)
      --> BinoHeap [Node 1 1 (Leaf ('h',1)) [Node 0 1 (Leaf ('e',1)) []]]
    insertInPrioQ (insertInPrioQ (insertInPrioQ PriorityQueue.empty ('h',1)) ('e',1)) ('l',2)
      --> BinoHeap [Node 0 2 (Leaf('l',2)) [],Node 1 1 (Leaf('h',1)) [Node 0 1 (Leaf('e',1)) []]]
    insertInPrioQ (insertInPrioQ (insertInPrioQ
      (insertInPrioQ PriorityQueue.empty ('h',1)) ('e',1)) ('l',2)) ('o',1)
      --> BinoHeap [Node 2 1 (Leaf ('h',1)) [Node 1 1 (Leaf ('o',1)) 
            [Node 0 2 (Leaf ('l',2)) []],Node 0 1 (Leaf ('e',1)) []]]
-}
insertInPrioQ :: PriorityQueue HuffmanTree -> (Char, Int) -> PriorityQueue HuffmanTree
insertInPrioQ priorityQ (character,occurrence) =
  PriorityQueue.insert priorityQ (Leaf (character,occurrence), priority)
    where priority = occurrence
-- - - - - - - - - - - - - - - - - - - - - - - - -
-- End of <huffmanTree> and its helper functions.
-- - - - - - - - - - - - - - - - - - - - - - - - -
-- Testing huffmanTree:
--  While the precise code for ' ' may vary, its length
-- (for the given example string) should always be 3 bits:
test1d = TestCase $ assertEqual "huffmanTree - hello"
          (Node(Leaf('l',2))5(Node(Leaf('h',1))3(Node(Leaf('o',1))2(Leaf('e',1)))))
          (huffmanTree (listToTable [('h',1),('e',1),('l',2),('o',1)]))
test1e = TestCase $ assertEqual "huffmanTree - xxx" (Leaf ('x',3))
          (huffmanTree (listToTable [('x', 3)]))
test1f = TestCase $ assertEqual "huffmanTree - empty string" (Leaf ('\NUL',0))
          (huffmanTree (listToTable [('\NUL',0)]))
testHuffmanTree2 = runTestTT $ TestList [test1,test1a,test1b,test1c,test1d,test1e,test1f]


-- - - - - - - - - - - - - - - - - - - - - - - - -
-- Start of <codeTable> and its helper functions.
-- - - - - - - - - - - - - - - - - - - - - - - - -
{- codeTable h
    Creates a table where each element contains a character
    and its encoded bit list corresponding to the given Huffman tree.
  PRE:     -
  RETURNS: a table that maps each character in h to its Huffman code.
  EXAMPLES:
    codeTable (huffmanTree $ characterCounts "xxx")
      --> T [('x',[])]
    codeTable (huffmanTree $ characterCounts "hello")
      --> T [('l',[False]),('e',[True,False]),('o',[True,True,False]),('h',[True,True,True])]
 -}
codeTable :: HuffmanTree -> Table Char BitCode
codeTable h = listToTable $ codeList h [] []

{- codeList huffTree list bitcode
    Helper function for creating a list where each element contains a character
    and its encoded bit list corresponding to the given Huffman tree.
    (The second argument is used as an accumulator for returning the final result.)
  RETURNS: a list that maps each character in h to its Huffman code
  EXAMPLES:
    codeList (huffmanTree $ characterCounts "xxx") [] []
      == [('x',[])]
    codeList (huffmanTree $ characterCounts "hello") [] []
      == [('h',[True,True,True]),('o',[True,True,False]),('e',[True,False]),('l',[False])]
 -}
codeList :: HuffmanTree -> [(Char, [Bool])] -> [Bool] -> [(Char, [Bool])]
-- VARIANT: size of huffTree - or - (the implicit) 'size' of (Node left v right)
-- Alternatively: the total number of edges not yet 'visited' in the Huffman tree ('huffTree')
-- Motivation: every recursive call corresponds to moving along one edge away from the root
-- of the Huffman tree until all leafs have been reached. All edges are 'visited' exactly once.
codeList (Node left v right) list bitcode = codeList right leftTable (True:bitcode)
  where leftTable = codeList left list (False:bitcode)
codeList (Leaf (char,_)) list bitcode = (char, reverse $ bitcode):list
-- - - - - - - - - - - - - - - - - - - - - - - -
-- End of <codeTable> and its helper functions.
-- - - - - - - - - - - - - - - - - - - - - - - -
-- Testing codeTable:
--  While the precise code for ' ' may vary, its length
-- (for the given example string) should always be 3 bits:
test2 = TestCase $ assertEqual "codeTable - long" 3 $ maybe (-1) length $ Table.lookup
  (codeTable (huffmanTree (characterCounts "this is an example of a huffman tree"))) ' '
test2a = TestCase $ assertEqual "codeTable - hello" 1
  $ maybe (-1) length $ Table.lookup (codeTable (huffmanTree (characterCounts "hello"))) 'l'
test2b = TestCase $ assertEqual "codeTable - xxx" 0
  $ maybe (-1) length $ Table.lookup (codeTable (huffmanTree (characterCounts "xxx"))) 'x'
testCodeTable3 = runTestTT $ TestList [test1,test1a,test1b,test1c,test1d,test2,test2a,test2b]


-- - - - - - - - - - - - - - - - - - - - - - - - -
-- Start of <compress> and its helper functions.
-- - - - - - - - - - - - - - - - - - - - - - - - -
{- compress s
    For the given string (s), creates a Huffman tree and the encoding of the message as a 
    list of bits, such that the encoding corresponds to the returned Huffman tree.
  PRE:     -
  RETURNS: (a Huffman tree based on s, the Huffman encoding of s under this tree)
  EXAMPLES:
    compress ""
      == (Leaf ('\NUL',0),[])
    compress "xxx"
      == (Leaf ('x',3),[])
    compress "hello"
      == (Node(Leaf('l',2))5(Node(Leaf('e',1))3(Node(Leaf('o',1))2(Leaf('h',1)))),
          [True,True,True,True,False,False,False,True,True,False])
    compress "treeeaa"
      == (Node(Leaf('e',3))7(Node(Leaf('a',2))4(Node(Leaf('t',1))2(Leaf('r',1)))),
          [True,True,False,True,True,True,False,False,False,True,False,True,False])
    compress "yyyrghyy"
      == (Node(Node(Leaf('g',1))3(Node(Leaf('r',1))2(Leaf('h',1)))) 8(Leaf('y',5)),
          [True,True,True,False,True,False,False,False,False,True,True,True,True])
-}
compress :: String -> (HuffmanTree, BitCode)
compress "" = (Leaf ('\NUL',0), [])
compress s  = (hTree, bitList)
  where hTree   = huffmanTree $ characterCounts s
        bitList = concat $ map (\x -> maybe [] id (Table.lookup (codeTable hTree) x)) s
-- - - - - - - - - - - - - - - - - - - - - - - -
-- End of <compress> and its helper functions.
-- - - - - - - - - - - - - - - - - - - - - - - -
-- Testing compress:
-- While the precise code for the given example string may vary,
-- its length should always be 135 bits:
test3 = TestCase $ assertEqual "compress" 135
  (length (snd (compress "this is an example of a huffman tree")))
test3a = TestCase $ assertEqual "compress"
  (Node(Leaf('l',2))5(Node(Leaf('e',1))3(Node(Leaf('o',1))2(Leaf('h',1)))),
  [True,True,True,True,False,False,False,True,True,False]) (compress "hello")
testCompress4 = runTestTT $ TestList
  [test1,test1a,test1b,test1c,test1d,test2,test2a,test2b,test3,test3a]


-- - - - - - - - - - - - - - - - - - - - - - - - -
-- Start of <decompress> and its helper functions.
-- - - - - - - - - - - - - - - - - - - - - - - - -
{- decompress h bits
    For the given Huffman tree and corresponding list of bits,
    decodes the original string.
  PRE:     bits is a concatenation of valid Huffman code words for h
  RETURNS: the decoding of bits under h
  EXAMPLES:
    decompress (Leaf ('\NUL',0)) []
      == ""
    decompress (Leaf ('x',3)) []
      == "xxx"
    hTree = huffmanTree $ characterCounts "hello"
    decompress hTree [True,True,True,True,False,False,False,True,True,False]
      == "hello"
    hTree = huffmanTree $ characterCounts "yyyrghyy"
    decompress hTree
        (concat $ map (\x -> maybe [] id (Table.lookup (codeTable hTree) x)) "yyyrghyy")
      == "yyyrghyy"
-}
decompress :: HuffmanTree -> BitCode -> String
decompress h bits = reverse $ decompressAux h h bits []

{- decompressAux h h bits []
    For the given Huffman tree and corresponding list of bits, decodes the
    original string in reverse order.
  PRE:     the first and second arguments must contain the same Huffman tree
           the third argument must be the bit list that corresponds to the given Huffman tree
           the fourth argument must be the empty list
  RETURNS: the decoding of bits under h in reverse order
  EXAMPLES:
    decompressAux (Leaf ('\NUL',0)) (Leaf ('\NUL',0)) [] []
      == ""
    hTree = huffmanTree $ characterCounts "xxx"
    decompressAux (Leaf ('x',3)) (Leaf ('x',3)) [] "F-ing whatever"
      == "xxx"
    hTree = huffmanTree $ characterCounts "hello"
    decompressAux hTree hTree (concat $ map (lookupBitCode $ codeTable hTree) "hello") []
      == "olleh"
-}
decompressAux :: HuffmanTree -> HuffmanTree -> BitCode -> String -> String
decompressAux (Leaf (singleChar, occurrence)) _ _ _ =
  replicate occurrence singleChar
decompressAux root (Node left v right) (False:bits) sReversed =
  decompressAux root left bits sReversed
decompressAux root (Node left v right) (True:bits) sReversed =
  decompressAux root right bits sReversed
decompressAux root (Leaf (char, thisIsTheLastLeaf)) [] sReversed = char:sReversed
decompressAux root (Leaf (char, prio)) bits sReversed =
  decompressAux root root bits (char:sReversed)
-- -- -- {- decompress h bits
-- -- --     For the given Huffman tree and corresponding list of bits,
-- -- --     decodes the original string.
-- -- --   PRE:     bits is a concatenation of valid Huffman code words for h.
-- -- --   RETURNS: the decoding of bits under h.
-- -- --   EXAMPLES:
-- -- --     decompress (Leaf ('\NUL',0)) []
-- -- --       == ""
-- -- --     decompress (Leaf ('x',3)) []
-- -- --       == "xxx"
-- -- --     hTree = huffmanTree $ characterCounts "lol"
-- -- --     decompress hTree [True,False,True]
-- -- --       == "lol"
-- -- --     hTree = huffmanTree $ characterCounts "hello"
-- -- --     decompress hTree [True,True,True,True,False,False,False,True,True,False]
-- -- --       == "hello"
-- -- --     hTree = huffmanTree $ characterCounts "yyyrghyy"
-- -- --     decompress hTree (concat $ map (lookupBitCode $ codeTable hTree) "yyyrghyy")
-- -- --       == "yyyrghyy"
-- -- -- -}
-- -- -- decompress :: HuffmanTree -> BitCode -> String
-- -- -- -- VARIANT: length bits (or length remainingBits)
-- -- -- decompress (Leaf (singleChar, occurrence)) _ = replicate occurrence singleChar
-- -- -- decompress h [] = []
-- -- -- decompress h bits = char:decompress h remainingBits
-- -- --   where (char,remainingBits) = decompressOneChar h bits


-- -- -- {- decompressOneChar h bits []
-- -- --     For the given Huffman tree and corresponding list of bits, decodes the
-- -- --     next (implicit) character in the given bit code list (bits).
-- -- --   PRE:  the first argument must be a valid Huffman tree.
-- -- --         The second argument must be a non-empty bit list that matches
-- -- --         the given Huffman tree. The third argument must be the empty list.
-- -- --   RETURNS: the decoding of _one_ character for the given h and bit code list.
-- -- --   EXAMPLES:
-- -- --     hTree = huffmanTree $ characterCounts "hello"
-- -- --     decompressOneChar hTree [True,True,True,True,False,False,False,True,True,False]
-- -- --       == ('h',[True,False,False,False,True,True,False])
-- -- --     decompressOneChar hTree [True,False,False,False,True,True,False]
-- -- --       == ('e',[False,False,True,True,False])
-- -- --     decompressOneChar hTree [False,True,True,False]
-- -- --       == ('l',[True,True,False])
-- -- --     decompressOneChar hTree [True,True,False]
-- -- --       == ('o',[])
-- -- -- -}
-- -- -- decompressOneChar :: HuffmanTree -> [Bool] -> (Char, [Bool])
-- -- -- -- VARIANT: size of huffTree - or - (the implicit) size of (Node left v right)
-- -- -- -- Alternatively: the (maximum) number of edges in the Huffman tree
-- -- -- --                remaining until a leaf is  encountered.
-- -- -- -- Motivation: every recursive call moves along an edge away from the root of the
-- -- -- -- Huffman tree until the leaf corresponding to the 'current' bit code has been reached.
-- -- -- decompressOneChar (Node left v right) (False:bits) = decompressOneChar left bits
-- -- -- decompressOneChar (Node left v right) (True:bits) = decompressOneChar right bits
-- -- -- decompressOneChar (Leaf (char, _)) bits = (char,bits)
-- - - - - - - - - - - - - - - - - - - - - - - -
-- End of <decompress> and its helper functions.
-- - - - - - - - - - - - - - - - - - - - - - - -


--------------------------------------------------------------------------------
-- Test Cases
-- You may add your own test cases here:
-- Follow the pattern and/or read about HUnit on the interwebs.
--------------------------------------------------------------------------------

-- Testing decompress:
test4 = TestCase $ assertEqual ("decompress '" ++ s ++ "'") s
  (let (h, bits) = compress s in decompress h bits)
    where s = "this is an example of a huffman tree"

test5 = TestCase $ assertEqual ("decompress '" ++ s ++ "'")
        s (let (h, bits) = compress s in decompress h bits)  where s = "xxx"

test6 = TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)  where s = ""

test6a = TestCase $ assertEqual ("decompress '" ++ s ++ "'")
        s (let (h, bits) = compress s in decompress h bits)  where s = "hello"

testQuickCheck =
  TestCase $ quickCheck (\s -> s == let (h, bits) = compress s in decompress h bits)

-- Running the six given tests:
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6]

-- Running ALL the tests:
testDecompress5 = runTestTT $ TestList [test1,test1a,test1b,test1c,test1d,test1e,test1f,
  test2,test2a,test2b,test3,test3a,test4,test5,test6,test6a,testQuickCheck]
