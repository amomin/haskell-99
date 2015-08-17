module Problem36 where

import Problem31(primesUpTo)
import Problem35(primeFactors)
import Problem10(encode)

primeFactorsMult :: Int -> [(Int,Int)]
primeFactorsMult m = reverse $ helper m (primesUpTo sqrtN) []
    where
        sqrtN = 1 + (ceiling $ sqrt $ fromIntegral m)
        helper 1 _ factors = factors
        helper n [] factors = (n,1):factors
        helper n ps@(p:pt) factors = 
            if mod n p == 0
                then let k = primeComponent n p in helper (quot n (p^k)) ps ((p,k):factors) 
                else helper n pt factors
        primeComponent n p = pchlpr n p 0
            where
                pchlpr n1 p1 k
                    | mod n1 p1 == 0 = pchlpr (quot n1 p1) p1 k+1
                    | otherwise = k

-- from solutions
-- use encode from problem 10 and solution to 35
primeFactorsMult2 :: Int -> [(Int,Int)]
primeFactorsMult2 n = map (\(x,y)->(y,x)) $ encode $ primeFactors n
