module Problem31 where

-- simplest, check if divisible by m for m = 2, odds up to sqrt n
isPrime1 n = isPrimeHelper 2 n
    where
        isPrimeHelper :: Int -> Int -> Bool
        isPrimeHelper m n
            | m == n = True
            | m == 2 = if ((mod n 2) /= 0) then (isPrimeHelper 3 n) else False
            | m < (ceiling $ sqrt $ fromIntegral n) = if ((mod n m) /= 0)
                                                        then (isPrimeHelper (m+2) n)
                                                        else False
            | otherwise = if ((mod n m) /= 0) then True else False

-- sieve of erathosthenes
-- really slow because it finds *all* primes
-- up to n
isPrime2 :: Int -> Bool
isPrime2 n = applyfltr [2..n]
    where
        sqrtn = ceiling $ sqrt $ fromIntegral n
        fltr y sieve = filter (\x -> (mod x y) /= 0) sieve 
        applyfltr sieve
            | (null sieve) = True
            | (head sieve) > sqrtn = elem n sieve
            | otherwise = applyfltr (fltr (head sieve) sieve)

-- mix of above two, even though it will
-- only check primes, still apparently slower
-- than first method
isPrime3 :: Int -> Bool
isPrime3 2 = True
isPrime3 n = hlpr [2..sqrtn] n
    where
        sqrtn = ceiling $ sqrt $ fromIntegral n
        fltr y lst = filter (\x -> (mod x y) /= 0) lst 
        hlpr possfacs n
            | (null possfacs) = True
            | (mod n (head possfacs)) == 0 = False
            | otherwise = hlpr (fltr (head possfacs) possfacs) n
