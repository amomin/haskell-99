module Problem12_Test where

import Test.HUnit
import Problem11
import Problem12

testEmpty :: Test
testEmpty = TestCase $ assertEqual
    "Empty list" [] (decodeModified emptyList)
        where
            emptyList :: [Record Char]
            emptyList = []

testSingle :: Test
testSingle = TestCase $ assertEqual
    "list with one Single record" "a" (decodeModified [(Single 'a')])

testMultiple :: Test
testMultiple = TestCase $ assertEqual
    "list with one Multiple record" "aaaa" (decodeModified [(Multiple 4 'a')])

testLeftInverse :: (Show a, Eq a) => [a] -> Test
testLeftInverse x = TestCase $ assertEqual
    "tests idempotency" x  (decodeModified $ encodeModified x)

--would need to implement Eq on Record a
--testRightInverse :: Show a => [Record a] -> Test
--testRightInverse x = TestCase $ assertEqual
    --"tests idempotency" x  (encode . decodeModified x)

main :: IO Counts
main = runTestTT $ TestList [testEmpty, testSingle, testMultiple,
    testLeftInverse "aaabbbaaaaabbbaacccabacaa"
    ]
