module Problem40 where

import Problem31(primesUpTo)

goldbach :: Int -> (Int, Int)
goldbach n | n < 2 = error "Must be a positive even integer"
goldbach n | (mod n 2) /= 0  = error "Must be a positive even integer"
goldbach 4 = (2,2)
goldbach n = ghlpr n $ tail $ primesUpTo n
    where
        ghlpr n [] = error "Could not find pair (probable error in algorithm"
        ghlpr n ps@(p:pt) = if (elem (n-p) ps) then (p,n-p) else ghlpr n pt
