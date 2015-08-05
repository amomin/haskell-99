module Problem01_Test where

import Problem01 (myLast)
import Test.HUnit

testSingleton :: Test
testSingleton = TestCase $ assertEqual
    "Should return only item in list [1]" 1 (myLast [1])

testCharArray :: Test
testCharArray = TestCase $ assertEqual
    "Should return 'z'" 'z' (myLast ['a', 'b', 'c', 'z'])

main :: IO Counts
main = runTestTT $ TestList [testSingleton, testCharArray]