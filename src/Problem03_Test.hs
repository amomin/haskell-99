module Problem03_Test where

import Problem03 (elementAt, elementAt')
import Test.HUnit

--testEmptyList :: (Eq a, Num a, Show a) => (Integer -> [a] -> a) -> Test
--testEmptyList f = TestCase $ assertError
    --"Error message" (f [] 1)

--testNegativeIndex :: (Eq a, Num a, Show a) => (Integer -> [a] -> a) -> Test
--testNegativeIndex f = TestCase $ assertError
    --"Error message" (f [] (-1))

--testOutOfBounds :: (Eq a, Num a, Show a) => (Integer -> [a] -> a) -> Test
--testOutOfBounds f = TestCase $ assertError
    --"Error meesage" (f [1,2] (3))


testSingleton :: (Eq a, Num a, Show a) => (Integer -> [a] -> a) -> Test
testSingleton f = TestCase $ assertEqual
    "Should return the only element in [1]" 1 (f 1 [1])

testDoubleton1 :: (Eq a, Num a, Show a) => (Integer -> [a] -> a) -> Test
testDoubleton1 f = TestCase $ assertEqual
    "Should return the first element in [1, 2]" 1 (f 1 [1, 2])

testDoubleton2 :: (Eq a, Num a, Show a) => (Integer -> [a] -> a) -> Test
testDoubleton2 f = TestCase $ assertEqual
    "Should return the second element in [1, 2]" 2 (f 2 [1, 2])

testLastElement :: (Eq a, Num a, Show a) => (Integer -> [a] -> a) -> Test
testLastElement f = TestCase $ assertEqual
    "Should return the last element in [3,4,5,6]" 6 (f 4 [3,4,5,6])

main :: IO Counts
testSchemas =  [testSingleton, testDoubleton1, testDoubleton2, testLastElement]
main = do
    putStrLn "Testing elementAt"
    runTestTT $ TestList $ map (\x -> (x elementAt)) testSchemas
    putStrLn "Testing elementAt'"
    runTestTT $ TestList $ map (\x -> (x elementAt')) testSchemas
