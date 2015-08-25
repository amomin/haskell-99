module Problem82_Test where


import NNPGraphs
import Problem82 -- thsi in turn requires Problem81 and NNPGraphs as well
import Test.HUnit


testFromProblemStatementGraph = Graph [1..6] [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
doubleCrossingGraph = Graph [1..6] [(1,2),(2,3),(3,4),(4,5),(5,3),(3,1)]

testSampleGraph :: Test
testSampleGraph  = TestCase $ assertEqual
    "Should have six solutions" 6 (length $ Problem82.cycle testFromProblemStatementGraph 2)

testSampleGraphNo2Path :: Test
testSampleGraphNo2Path  = TestCase $ assertEqual
    "Should have no solution of length 2" True (all (> 2) $ map length $ Problem82.cycle testFromProblemStatementGraph 2)

testSampleGraphAnswer1 :: Test
testSampleGraphAnswer1  = TestCase $ assertEqual
    "Should contain [2,1,3,2]" True (elem [2,1,3,2] $ Problem82.cycle testFromProblemStatementGraph 2)

testDoubleCross :: Test
testDoubleCross  = TestCase $ assertEqual
    "Should not(?) contain [1,2,3,4,5,3,1]" False (elem [1,2,3,4,5,3,1] $ Problem82.cycle doubleCrossingGraph 1)

main :: IO Counts
main = runTestTT $ TestList [testSampleGraph, testSampleGraph, testSampleGraphNo2Path, testDoubleCross]
