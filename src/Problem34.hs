module Problem34 where

import Problem31(primesUpTo)
import Problem32(myGCD)
import Problem33(coprime)

-- trivial (but slow) solution given problems 32/33
totient1 :: Int -> Int
totient1 n = length $ filter (coprime n) [1..n]
-- equivalently
--totient1 n = length $ filter (\x -> (myGCD x n) == 1) [1..n]

-- another solution using the multiplicative property
-- but not efficient because it uses all primes up to n
-- instead of only up to the first prime encountered
totient2 :: Int -> Int
totient2 n = product (map (totientHelper n 0) primesList)
    where
        --primesList = primesUpTo $ ceiling $ sqrt $ fromIntegral n
        primesList = primesUpTo n
        primeTotient :: Int -> Int -> Int
        primeTotient p 0 = 1
        primeTotient p 1 = p - 1
        primeTotient p k = p * (primeTotient p (k-1))
        totientHelper :: Int -> Int -> Int -> Int
        totientHelper n k p
            | (mod n p) == 0    = totientHelper (quot n p) (k+1) p
            | otherwise         = primeTotient p k

-- another solution using the multiplicative property
totient3 :: Int -> Int
totient3 1 = 1
totient3 n = (primeTotient p k) * totient3 (quot n (p^k))
    where
        p = head $ dropWhile (coprime n) [2..]
        k = numExponent p n
        numExponent :: Int -> Int -> Int
        numExponent p n = helper p n 0
            where
                helper p n k
                    | (mod n p) == 0    = helper p (quot n p) (k+1)
                    | otherwise         = k
        primeTotient :: Int -> Int -> Int
        primeTotient p 0 = 1
        primeTotient p 1 = p - 1
        primeTotient p k = p * (primeTotient p (k-1))

-- as above but passing the list of dropped primes
-- to improve performance again
-- doesn't seem to have much effect though
totient4 :: Int -> Int
totient4 n = t4hlpr n [2..n]
    where
        t4hlpr :: Int -> [Int] -> Int
        t4hlpr 1 _ = 1
        t4hlpr n lst = (primeTotient p k) * (t4hlpr (quot n (p^k)) redLst)
            where
                (p:redLst) = dropWhile (coprime n) lst
                k = numExponent p n
                numExponent :: Int -> Int -> Int
                numExponent p n = helper p n 0
                    where
                        helper p n k
                            | (mod n p) == 0    = helper p (quot n p) (k+1)
                            | otherwise         = k
                primeTotient :: Int -> Int -> Int
                primeTotient p 0 = 1
                primeTotient p 1 = p - 1
                primeTotient p k = p * (primeTotient p (k-1))
