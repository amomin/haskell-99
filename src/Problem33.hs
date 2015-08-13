module Problem32 where

import Problem33(myGCD)

coprime :: Int -> Int -> Int
coprime x y = (myGCD x y) == 1
