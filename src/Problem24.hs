module Problem24 where

import System.Random (randomRIO)
import Problem20 (removeAt)
import Problem23 (rndSelect23)

rndSelect24 :: Int -> Int -> IO [Int]
rndSelect24 a b = rndSelect23 [1..a] b
