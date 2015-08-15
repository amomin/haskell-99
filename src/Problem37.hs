module Problem37 where

import Problem36

-- recursive solution
totient1 :: Int -> Int
totient1 n = helper (primeFactorsMult n)
    where
        helper [] = 1
        helper ps@((p,k):pt) = (p-1)*(p^(k-1))*(helper pt) 

-- basically the same solution using product and map instead
totient2 :: Int -> Int
totient2 n = product $ map (\(p,k) -> (p-1)*(p^(k-1))) $ primeFactorsMult n

-- suggested solution using list comprehension
totient3 :: Int -> Int
totient3 n = product [(p-1)*(p^(k-1)) | (p,k) <-  primeFactorsMult n]
