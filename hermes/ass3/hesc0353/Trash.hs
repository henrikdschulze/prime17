test6a = TestCase $ assertEqual ("decompress '" ++ s ++ "'")
        s (let (h, bits) = compress s in decompress h bits)  where s = "hello"

-- -- -- runTestTT $ TestList [TestCase $ assertEqual ("decompress '" ++ s ++ "'")
-- -- --         s (let (h, bits) = compress s in decompress h bits)  where s = "hello"]

let s = "hello"; (h, bits) = compress s in decompress h bits

testFoo = TestCase $ assertEqual ("decompress '" ++ s ++ "'")
        s (let (h, bits) = compress s in decompress h bits)  where s = "hello"

testBar = 

runTestTT $ TestList [TestCase $ assertEqual "test" "hello" 
  (let (h, bits) = compress "hello" in decompress h bits)]

-- -- -- runTestTT $ TestList [TestCase $ assertEqual "hello" s 
-- -- --   (let (h, bits) = compress s in decompress h bits) where s = "hello"]

runTestTT $ TestList [TestCase $ assertEqual "test" "hello" (let (h, bits) = compress "hello" in decompress h bits)]

assertEqual "test hello" "hello" (let (h, bits) = compress "hello" in decompress h bits)

quickCheck ((\s -> let (h, bits) = compress s in decompress h bits == s) :: [Char] -> Bool)

quickCheck ((\s -> (reverse . reverse) s == s) :: [Char] -> Bool)

quickCheck (\s -> (reverse . reverse) s == s)

quickCheck (\s -> s == (reverse . reverse) s)

runTestTT $ TestList [TestCase $ quickCheck (\s -> s == (reverse . reverse) s)]

quickCheck (\s -> s == let (h, bits) = compress s in decompress h bits)

runTestTT $ TestList [TestCase $ quickCheck (\s -> s == (let (h, bits) = compress s in decompress h bits))]


maybe (-1) length $ Table.lookup (codeTable (huffmanTree (characterCounts "hello"))) 'h'
length $ Table.lookup (codeTable (huffmanTree (characterCounts "hello"))) 'x'

maybe (-1) length $ Table.lookup (codeTable (huffmanTree (characterCounts "xxx"))) 'x'
length $ Table.lookup (codeTable (huffmanTree (characterCounts "xxx"))) 'x'
Table.lookup (codeTable (huffmanTree (characterCounts "xxx"))) 'x'

maybe [] $ Table.lookup (codeTable (huffmanTree (characterCounts "xxx"))) 'x'

