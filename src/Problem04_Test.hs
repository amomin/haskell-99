module Problem04_Test where

import Problem04 (myLength, myLength', myLength'', myLength''', myLength'''')
import Test.HUnit

testEmpty :: ([a] -> Int) -> Test
testEmpty f = TestCase $ assertEqual
    "Length of empty list" 0 (f [])

testSingletonInt :: ([Int] -> Int) -> Test
testSingletonInt f = TestCase $ assertEqual
    "Length of list of one integer" 1 (f [5])

-- Getting in trouble with the type signatures that i don't understand yet
-- Wanted ([a] -> Int) -> Test but kept getting an error, this below
-- does not work either
--testSingletonChar :: ([Char] -> Int) -> Test
--testSingletonChar f = TestCase $ assertEqual
    --"Length of list of one char" 1 (f ['c'])

--testSingletonCharString :: ([Char] -> Int) -> Test
--testSingletonCharString f = TestCase $ assertEqual
    --"Length of list of one char" 1 (f "c")

testFiveElementList :: ([Int] -> Int) -> Test
testFiveElementList f = TestCase $ assertEqual
    "Length of 5 int element list" 5 (f [7,8,9,1,2])

--testFiveElementString :: ([Char] -> Int) -> Test
--testFiveElementString f = TestCase $ assertEqual
    --"Length of 5 char string" 5 (f "tuvwx")

main :: IO Counts
--testSchemas =  [testEmpty, testSingletonInt, testSingletonChar, testSingletonCharString, testFiveElementList, testFiveElementString];
testSchemas =  [testEmpty, testSingletonInt, testFiveElementList];
--testSchemas =  [testEmpty]
main = do
    putStrLn "Testing myLength"
    runTestTT $ TestList $ map (\x -> (x myLength)) testSchemas
    putStrLn "Testing myLength'"
    runTestTT $ TestList $ map (\x -> (x myLength')) testSchemas
    putStrLn "Testing myLength''"
    runTestTT $ TestList $ map (\x -> (x myLength'')) testSchemas
    putStrLn "Testing myLength'''"
    runTestTT $ TestList $ map (\x -> (x myLength''')) testSchemas
    putStrLn "Testing myLength''''"
    runTestTT $ TestList $ map (\x -> (x myLength'''')) testSchemas
