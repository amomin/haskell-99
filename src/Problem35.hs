module Problem35 where

import Problem31(primesUpTo)

primeFactors :: Int -> [Int]
primeFactors n = reverse $ helper n (primesUpTo sqrtN) []
    where
        sqrtN = 1 + (ceiling $ sqrt $ fromIntegral n)
        helper 1 _ factors = factors
        helper n [] factors = (n:factors)
        helper n ps@(p:pt) factors = 
            if (mod n p) == 0
                then helper (quot n p) ps (p:factors)
                else helper n pt factors
    
