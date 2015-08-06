module Problem05_Test where

import Problem05
import Test.HUnit

testSchema :: (Show a, Eq a) => ([a] -> [a]) -> [a] -> Test
testSchema f x = TestCase $ assertEqual
    "Failed test" (reverse x) (f x)

testEmpty f = testSchema f []
testSingleInt f = testSchema f [1]
--testSingleChar f = testSchema f ['c']
--testSingleCharString f = testSchema f "c"
testInts1 f = testSchema f [5..22]
testInts2 f = testSchema f [77..214]

main :: IO Counts
testSchemas =  [testEmpty, testSingleInt, testInts1, testInts2]
--testSchemas =  [testEmpty]
main = do
    putStrLn "Testing myReverse"
    runTestTT $ TestList $ map (\x -> (x myReverse)) testSchemas
    putStrLn "Testing myReverse1"
    runTestTT $ TestList $ map (\x -> (x myReverse1)) testSchemas
    putStrLn "Testing myReverse2"
    runTestTT $ TestList $ map (\x -> (x myReverse2)) testSchemas
    putStrLn "Testing myReverse3"
    runTestTT $ TestList $ map (\x -> (x myReverse3)) testSchemas
    putStrLn "Testing myReverse4"
    runTestTT $ TestList $ map (\x -> (x myReverse4)) testSchemas
