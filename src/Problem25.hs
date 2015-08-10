module Problem25 where

import System.Random (randomRIO)
import Problem20 (removeAt)
import Problem23 (rndSelect23)

rndPermutation :: [a] -> IO [a]
rndPermutation xs = rndSelect23 xs (length xs)
