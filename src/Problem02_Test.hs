module Problem02_Test where

import Problem02 (myButLast)
import Test.HUnit

testDoubleton :: Test
testDoubleton = TestCase $ assertEqual
    "Should return only item in list [1, 2]" 1 (myButLast [1, 2])

testCharArray :: Test
testCharArray = TestCase $ assertEqual
    "Should return 'c'" 'c' (myButLast ['a', 'b', 'c', 'z'])

main :: IO Counts
main = runTestTT $ TestList [testDoubleton, testCharArray]